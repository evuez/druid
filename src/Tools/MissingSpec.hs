module Tools.MissingSpec where

import Expr.Concrete (ExprW)

run :: ExprW -> ExprW
run wE = do
  e <- wE
  pure e
