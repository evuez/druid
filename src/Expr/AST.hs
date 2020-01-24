module Expr.AST (Expr(..)) where

import Data.List (intercalate)

data Expr
  = Pair (Expr, Expr)
  | Triple (Expr, Expr, Expr)
  | Keywords [(Expr, Expr)]
  | List [Expr]
  | Atom String
  | Integer Integer
  | Float Float
  | String String
  | Charlist String

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Pair (a, b)) = concat ["{", show a, ", ", show b, "}"]
showExpr (Triple (a, b, c)) =
  concat ["{:{}, [], [", show a, ", ", show b, ", ", show c, "]}"]
showExpr (Keywords pairs) =
  concat ["[", intercalate ", " $ show <$> pairs, "]"]
showExpr (List a) = show a
showExpr (Atom a) = concat [":", a]
showExpr (Integer a) = show a
showExpr (Float a) = show a
showExpr (String a) = a
showExpr (Charlist a) = a
