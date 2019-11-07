module ElaboratorImpl where

import Absyn
-- add other imports

lookres :: [Resource] -> RName -> Either ErrMsg Resource
lookres [] rname = Left ("Couldn't find a resource with the name: " ++ rname)
lookres r:res rname = undefined

elaborate :: IDB -> Either ErrMsg DB
elaborate = undefined
