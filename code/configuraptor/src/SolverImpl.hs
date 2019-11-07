module SolverImpl where

import Absyn
import Data.Char
import qualified Data.List as L

combine :: RProf -> RProf -> RProf
combine [] rProf2 = rProf2
combine rProf1 [] = rProf1
combine (res1:rProf1) (res2:rProf2) = do
  let (R rName1, (pro1, req1)) = res1
  let (R rName2, (pro2, req2)) = res2
  if rName1 < rName2
    then res1:combine rProf1 (res2:rProf2)
    else if rName1 > rName2
      then res2:combine (res1:rProf1) rProf2
      else do
        let pro3 = pro1 + pro2
        let req3 = max req1 req2
        if (pro3, req3) == (0,0)
        then combine rProf1 rProf2
        else (R rName1, (pro3,req3)):combine rProf1 rProf2

verify :: DB -> Goal -> Solution -> Either ErrMsg RProf
verify db goal sol = do
  let l1 = length sol
  let cnames = map (\(x, _) -> x) sol
  let l2 = length (L.nub (map (\x -> map (\y -> toLower y) x) cnames)) -- nub removes duplicates
  if l1 > l2
    then Left "The solution contains the same component multiple times"
    else if L.all (\x -> x > 0) (map (\(_, x) -> x) sol)
      then verify2 db goal sol
      else Left "One of the components have a negative value"

verify2 :: DB -> Goal -> Solution -> Either ErrMsg RProf
verify2 db goal [] =
  if all (\(_, (x, y)) -> x >= y) goal --requirement y can only be 0 or higher
    then Right goal
    else Left "Solution isn't reachable from goal"
verify2 (res, comp) goal ((cname, n):sol) =
  case lookupdb comp cname of
    Right rprof -> verify2 (res, comp) (combine goal (ncombine rprof n)) sol
    err -> err

-- returns RProf from the component part of the database
lookupdb :: [(CName, RProf)] -> CName -> Either ErrMsg RProf
lookupdb [] cname = Left ("Couldn't find the component: '" ++ cname ++ "' in the database.")
lookupdb ((dbcname, rprof):db) cname =
  if cname == dbcname
    then Right rprof
    else lookupdb db cname

-- returns RProf multiplied by the second argument
ncombine :: RProf -> Int -> RProf
ncombine rprof 1 = rprof
ncombine rprof n = combine rprof (ncombine rprof (n-1))

solve :: DB -> Goal -> Int -> Either ErrMsg Solution
solve = undefined
