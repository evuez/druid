module Lib
  ( parseAST
  , readAST
  , reify
  ) where

import Control.Monad.Writer (writer)
import qualified Expr as E (AExpr(..), EExpr(..), Meta(..), WExpr)
import qualified Parser as P (ParseError, parser)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

--
-- Reify
--
reify :: E.AExpr -> E.WExpr
reify (E.AAtom a) = withMeta [] (E.Atom a)
reify (E.AFloat a) = withMeta [] (E.Float a)
reify (E.AInteger a) = withMeta [] (E.Integer a)
reify (E.AList a) = withMeta [] (E.List $ reify <$> a)
reify (E.AKeywords a) =
  withMeta
    []
    (E.List $ (\(x, y) -> withMeta [] (E.Tuple [reify x, reify y])) <$> a)
reify (E.AString a) = withMeta [] (E.String a)
reify (E.APair (a, b)) = withMeta [] (E.Tuple [reify a, reify b])
reify (E.ATriple (E.AAtom name, E.AKeywords meta, E.AAtom "Elixir")) =
  withMeta meta (E.Variable name)
reify (E.ATriple (E.AAtom name, E.AKeywords meta, E.AAtom "nil")) =
  withMeta meta (E.Variable name)
reify (E.ATriple (E.AAtom "__block__", E.AKeywords meta, E.AList exprs)) =
  withMeta meta (E.Block $ reify <$> exprs)
reify (E.ATriple (E.AAtom "__aliases__", E.AKeywords meta, E.AList (a:aliases))) =
  withMeta meta (E.Alias (reify a, fmap raw aliases))
reify (E.ATriple (E.AAtom "{}", E.AKeywords meta, E.AList exprs)) =
  withMeta meta (E.Tuple $ reify <$> exprs)
reify (E.ATriple (E.AAtom "%{}", E.AKeywords meta, E.AList [])) =
  withMeta meta (E.Map [])
reify (E.ATriple (E.AAtom "%{}", E.AKeywords meta, E.AKeywords kv)) =
  withMeta meta (E.Map $ both reify <$> kv)
reify (E.ATriple (E.AAtom "<<>>", E.AKeywords meta, E.AList exprs)) =
  withMeta meta (E.Binary $ reify <$> exprs)
reify (E.ATriple (E.AAtom "fn", E.AKeywords meta, E.AList exprs)) =
  withMeta meta (E.Fn $ reify <$> exprs)
reify (E.ATriple (E.ATriple (E.AAtom ".", E.AKeywords _, E.AList [lhs, E.AAtom rhs]), E.AKeywords meta, E.AList args)) =
  withMeta meta (E.QualifiedCall (reify lhs) rhs (reify <$> args))
reify (E.ATriple (E.ATriple (E.AAtom ".", E.AKeywords _, E.AList [expr]), E.AKeywords meta, E.AList args)) =
  withMeta meta (E.AnonymousCall (reify expr) (reify <$> args))
reify (E.ATriple (E.AAtom name, E.AKeywords meta, E.AList args)) =
  withMeta meta (E.NonQualifiedCall name (reify <$> args))
reify (E.ATriple (E.APair a, E.AKeywords meta, E.AList args)) =
  withMeta
    meta
    (E.Tuple [reify $ E.APair a, withMeta [] (E.List $ reify <$> args)])
reify (E.ATriple (E.ATriple a, E.AKeywords meta, E.AList args)) =
  withMeta
    meta
    (E.Tuple [reify $ E.ATriple a, withMeta [] (E.List $ reify <$> args)])
reify (E.ATriple (E.AAtom name, E.AKeywords meta, E.AKeywords args)) =
  withMeta meta (E.NonQualifiedCall name (reifyTuple <$> args))

--reify (E.ATriple (E.APair a, E.AKeywords meta, E.AKeywords args)) = E.Tuple [reify $ E.APair a, E.List $ reify <$> meta, E.List $ reify <$> args]
--reify (E.ATriple (E.ATriple a, E.AKeywords meta, E.AKeywords args)) = E.Tuple [reify $ E.ATriple a, E.List $ reify <$> meta, E.List $ reify <$> args]
reifyTuple :: (E.AExpr, E.AExpr) -> E.WExpr
reifyTuple (x, y) = withMeta [] (E.Tuple [reify x, reify y])

raw :: E.AExpr -> String
raw (E.AAtom a) = a

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

withMeta :: [(E.AExpr, E.AExpr)] -> E.EExpr -> E.WExpr
withMeta m e = writer (e, metaOrEmpty m)

metaOrEmpty :: [(E.AExpr, E.AExpr)] -> E.Meta
metaOrEmpty ((E.AAtom "line", E.AInteger line):_) = E.Meta line
metaOrEmpty (_:xs) = metaOrEmpty xs
metaOrEmpty [] = E.Empty

parseAST :: String -> Either P.ParseError E.AExpr
parseAST = parse P.parser ""

--
-- REPL
--
readAST :: String -> IO ()
readAST s =
  case parse P.parser "" s of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show (reify ast) ++ "\n"
