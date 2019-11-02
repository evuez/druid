module Lib where

import qualified Expr as E (EExpr(..))
--  ( readAST, parseAST
--  )
import qualified Parser as P (ParseError, parser)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

--
-- Specialize
--
-- E.Expr -> Reader E.Expr (Expr + metadata)
specialize :: E.EExpr -> E.EExpr
specialize (E.Atom a) = E.Atom a
specialize (E.Float a) = E.Float a
specialize (E.Integer a) = E.Integer a
specialize (E.List a) = E.List a
specialize (E.String a) = E.String a
specialize (E.Tuple [a, b]) = E.Tuple [a, b]
specialize (E.Tuple [E.Atom "__block__", E.List meta, E.List exprs]) =
  E.List $ specialize <$> exprs
specialize (E.Tuple [E.Atom "__aliases__", E.List meta, E.List (a:aliases)]) =
  E.Alias (raw a : (raw <$> aliases))
specialize (E.Tuple [E.Atom name, E.List meta, E.List args]) =
  E.NonQualifiedCall name args
-- E.Atom ".", ... RemoteCall
-- E.Atom ".", ... Fn
specialize (E.Tuple [E.Tuple a, E.List meta, E.List args]) =
  E.Tuple [specialize (E.Tuple a), E.List meta, E.List args]

raw :: E.EExpr -> String
raw (E.Atom a) = a
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
