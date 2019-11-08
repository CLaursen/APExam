module ElaboratorImpl where

import Absyn
import Data.Char
-- add other imports

lookres :: [Resource] -> RName -> Either ErrMsg Resource
lookres [] rname = Left ("Couldn't find a resource with any casing of the name: " ++ rname)
lookres ((R r):res) rname = if lowercase r == lowercase rname
                        then Right (R r)
                        else lookres res rname

lowercase :: String -> String
lowercase s = map (\x -> toLower x) s

elaborate :: IDB -> Either ErrMsg DB
elaborate = undefined
