module Lib where

import qualified Expr as E (EExpr(..))

--  ( readAST, parseAST
--  )
import qualified Parser as P (ParseError, parser)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

--
-- Compact
--
-- E.Expr -> Reader E.Expr (Expr + metadata)
compact :: E.EExpr -> E.EExpr
compact (E.Atom a) = E.Atom a
compact (E.Float a) = E.Float a
compact (E.Integer a) = E.Integer a
compact (E.List a) = E.List a
compact (E.String a) = E.String a
compact (E.Tuple [a, b]) = E.Tuple [a, b]
compact (E.Tuple [E.Atom name, E.List meta, E.Atom "Elixir"]) = E.Variable name
compact (E.Tuple [E.Atom "__block__", E.List meta, E.List exprs]) =
  E.Block $ compact <$> exprs
compact (E.Tuple [E.Atom "__aliases__", E.List meta, E.List (a:aliases)]) =
  E.Alias (raw a : fmap raw aliases)
compact (E.Tuple [E.Atom "{}", E.List meta, E.List exprs]) = E.Tuple exprs
compact (E.Tuple [E.Atom "%{}", E.List meta, E.List []]) = E.Map []
--compact (E.Tuple [E.Atom "%{}", E.List meta, E.List exprs@[(_, _)]]) = E.Map exprs
compact (E.Tuple [E.Atom "<<>>", E.List meta, E.List exprs]) = E.Binary exprs
compact (E.Tuple [E.Atom name, E.List meta, E.List args]) =
  E.NonQualifiedCall name args
-- E.Atom ".", ... RemoteCall
-- E.Atom ".", ... Fn
compact (E.Tuple [E.Tuple a, E.List meta, E.List args]) =
  E.Tuple [compact (E.Tuple a), E.List meta, E.List args]

raw :: E.EExpr -> String
raw (E.Atom a) = a
raw (E.Tuple [E.Atom "__MODULE__", _, E.Atom "Elixir"]) = "CurrentModule"
raw _ = "MODULE"

parseAST :: String -> Either P.ParseError E.EExpr
parseAST = parse P.parser ""

--
-- REPL
--
readAST :: String -> IO ()
readAST s =
  case parse P.parser "" s of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show ast ++ "\n"
