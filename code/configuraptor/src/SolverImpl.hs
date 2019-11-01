module SolverImpl where

import Absyn

combine :: RProf -> RProf -> RProf
combine = undefined

verify :: DB -> Goal -> Solution -> Either ErrMsg RProf
verify = undefined

solve :: DB -> Goal -> Int -> Either ErrMsg Solution
solve = undefined
