module ParserImpl where

import Absyn
import Text.ParserCombinators.ReadP
import Data.Char

parseString :: String -> Either String IDB
parseString s =
  case readP_to_S pDatabase s of
    [] -> Left "couldn't parse input"
    [(output, "")] -> Right output

pDatabase :: ReadP IDB
pDatabase = do
  remover()
  declz <- pDeclz ([], [])
  eof
  return declz

pDeclz :: IDB -> ReadP IDB
pDeclz (res, comp) = do
    ressource <- pRessourceDecl
    pDeclz (res ++ ressource, comp)
  +++ do
    component <- pComponentDecl
    pDeclz (res, comp ++ [component])
  <++
    return (res, comp)

pRessourceDecl :: ReadP [RName]
pRessourceDecl = do
  keyword "resource"
  rNames <- pRNames []
  symbol '.'
  return rNames

pRNames :: [RName] -> ReadP [RName]
pRNames rnList = do
    rName <- pRName
    symbol ','
    pRNames (rnList ++ [rName])
  +++ do
    rName <- pRName
    return (rnList ++ [rName])

pRName :: ReadP RName
pRName = pName ""

pName :: String -> ReadP String
pName name = do
    word <- pWord
    symbolString '-'
    pName (name ++ word ++ "-")
  +++ do
    word <- pWord
    let final = name ++ word
    if length (final) < 33
      then remover final
      else fail "name is too long"

pWord :: ReadP String
pWord = do
  word <- munch1 (\x -> (isLetter x && isAscii x) || isDigit x)
  let letters = filter (\x -> isLetter x && isAscii x) word
  if length letters /= 0
    then return word
    else fail "word doesn't contain any letters"

pComponentDecl :: ReadP IComp
pComponentDecl = do
  keyword "component"
  cName <- pCName
  symbol ':'
  clauses <- pClauses []
  symbol '.'
  return (IC cName clauses)

pCName :: ReadP CName
pCName = pName ""

pClauses :: [Clause] -> ReadP [Clause]
pClauses clauses = do
    clause <- pClause
    symbol ';'
    pClauses (clauses ++ [clause])
  +++ do
    clause <- pClause
    return (clauses ++ [clause])

pClause :: ReadP Clause
pClause = do
    keyword "provides"
    rSpec <- pRSpec
    return (CKProvides, rSpec)
  +++ do
    keyword "uses"
    rSpec <- pRSpec
    return (CKUses, rSpec)
  +++ do
    keyword "requires"
    rSpec <- pRSpec
    return (CKRequires, rSpec)

pRSpec :: ReadP RSpec
pRSpec = do
  term <- pTerm
  pRSpec2 term

pRSpec2 :: RSpec -> ReadP RSpec
pRSpec2 rSpec = do
    symbol '|'
    term <- pTerm
    rSpec3 <- pRSpec3 term
    pRSpec2 (RSOr rSpec rSpec3)
  <++
    pRSpec3 rSpec

pRSpec3 :: RSpec -> ReadP RSpec
pRSpec3 rSpec = do
    symbol ','
    term <- pTerm
    pRSpec2 (RSAnd rSpec term)
  <++
    return rSpec

pTerm :: ReadP RSpec
pTerm = do
    name <- pRName
    return (RSRes name)
  +++ do
    num <- pNum
    rSpec <- pTerm
    return (RSNum num rSpec)
  +++ do
    symbol '('
    rSpec <- pRSpec
    symbol ')'
    return rSpec

pNum :: ReadP Int
pNum = do
  string <- munch1 isDigit
  let num = read string
  if num < 1000000
    then do
      check "a number"
      return num
    else fail ("number is to large")

-- Code taken from AP19 Assignment 2 submission by KFN536+XJV552
keyword :: String -> ReadP ()
keyword s = do
  mapM_ (\c -> satisfy (\x -> x == toLower c || x == toUpper c)) s
  check s

check :: String -> ReadP ()
check s = do
  l <- look
  if null l
  then return ()
  else
    let c = head l in
      if not ((isLetter c && isAscii c) || c == '-' || isDigit c)
      then do
        remover ()
      else fail ("expected: " ++ s)

remover :: a -> ReadP a
remover a = do
    skipSpaces
    comment
    remover a
  <++ do
    skipSpaces
    return a

-- End of copied code


comment :: ReadP ()
comment = do
  symbolString '{'
  munch (\x -> x /= '{' && x /= '}')
  internalComments
  symbolString '}'
  return ()

internalComments :: ReadP ()
internalComments = do
    comment
    munch (\x -> x /= '{' && x /= '}')
    internalComments
    return ()
  <++
    return ()

-- Code taken from AP19 Assignment 2 submission by KFN536+XJV552

-- symbolString verifies that a given symbol is the next char we should parse
-- is used for names and comments
symbolString :: Char -> ReadP ()
symbolString c = do
  c' <- satisfy (== c)
  if c' == c then return ()
    else fail ("expected: " ++ [c] ++ " but got: " ++ [c'])

-- symbol does the same job as symbolString, but is not used inside strings,
-- so therefore it removes whitespace
symbol :: Char -> ReadP ()
symbol c = do
  c' <- satisfy (== c)
  if c' == c then remover ()
    else fail ("expected: " ++ [c] ++ " but got: " ++ [c'])

-- End of copied code