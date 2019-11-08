module Lib where

import qualified Expr as E (AExpr(..), EExpr(..))

--  ( readAST, parseAST
--  )
import qualified Parser as P (ParseError, parser)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

--
-- Reify
--
-- E.Expr -> Reader E.Expr (Expr + metadata)
reify :: E.AExpr -> E.EExpr
reify (E.AAtom a) = E.Atom a
reify (E.AFloat a) = E.Float a
reify (E.AInteger a) = E.Integer a
reify (E.AList a) = E.List $ reify <$> a
reify (E.AKeywords a) = E.List $ (\(x, y) -> E.Tuple [reify x, reify y]) <$> a
reify (E.AString a) = E.String a
reify (E.APair (a, b)) = E.Tuple [reify a, reify b]
reify (E.ATriple (E.AAtom name, E.AList meta, E.AAtom "Elixir")) =
  E.Variable name
reify (E.ATriple (E.AAtom "__block__", E.AList meta, E.AList exprs)) =
  E.Block $ reify <$> exprs
reify (E.ATriple (E.AAtom "__aliases__", E.AList meta, E.AList (a:aliases))) =
  E.Alias (reify a, fmap raw aliases)
reify (E.ATriple (E.AAtom "{}", E.AList meta, E.AList exprs)) =
  E.Tuple $ reify <$> exprs
reify (E.ATriple (E.AAtom "%{}", E.AList meta, E.AList [])) = E.Map []
reify (E.ATriple (E.AAtom "%{}", E.AList meta, E.AKeywords kv)) =
  E.Map $ both reify <$> kv
reify (E.ATriple (E.AAtom "<<>>", E.AList meta, E.AList exprs)) =
  E.Binary $ reify <$> exprs
reify (E.ATriple (E.AAtom "fn", E.AList meta, E.AList exprs)) =
  E.Fn $ reify <$> exprs
reify (E.ATriple (E.ATriple (E.AAtom ".", E.AList meta', E.AList [lhs, E.AAtom rhs]), E.AList meta, E.AList args)) =
  E.QualifiedCall (reify lhs) rhs (reify <$> args)
reify (E.ATriple (E.ATriple (E.AAtom ".", E.AList meta', E.AList [expr]), E.AList meta, E.AList args)) =
  E.AnonymousCall (reify expr) (reify <$> args)
reify (E.ATriple (E.AAtom name, E.AList meta, E.AList args)) =
  E.NonQualifiedCall name (reify <$> args)
-- E.Atom ".", ... RemoteCall
-- E.Atom ".", ... Fn
reify (E.ATriple (E.APair a, E.AList meta, E.AList args)) =
  E.Tuple [reify $ E.APair a, E.List $ reify <$> meta, E.List $ reify <$> args]
reify (E.ATriple (E.ATriple a, E.AList meta, E.AList args)) =
  E.Tuple
    [reify $ E.ATriple a, E.List $ reify <$> meta, E.List $ reify <$> args]
reify (E.ATriple (E.AAtom name, E.AList meta, E.AKeywords args)) =
  E.NonQualifiedCall name (reifyTuple <$> args)

--reify (E.ATriple (E.APair a, E.AList meta, E.AKeywords args)) = E.Tuple [reify $ E.APair a, E.List $ reify <$> meta, E.List $ reify <$> args]
--reify (E.ATriple (E.ATriple a, E.AList meta, E.AKeywords args)) = E.Tuple [reify $ E.ATriple a, E.List $ reify <$> meta, E.List $ reify <$> args]
reifyTuple :: (E.AExpr, E.AExpr) -> E.EExpr
reifyTuple (x, y) = E.Tuple [reify x, reify y]

raw :: E.AExpr -> String
raw (E.AAtom a) = a

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

parseAST :: String -> Either P.ParseError E.AExpr
parseAST = parse P.parser ""

--
-- REPL
--
readAST :: String -> IO ()
readAST s =
  case parse P.parser "" s of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show ast ++ "\n"
