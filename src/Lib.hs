module Lib
  ( parseAST
  , readAST
  , reify
  ) where

import Control.Monad.Writer (writer)
import qualified Expr.AST as A (Expr(..))
import qualified Expr.Concrete as C (BlockVal(..), Expr(..), WExpr)
import qualified Meta as M (Meta(..))
import qualified Parser as P (ParseError, parser)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

--
-- Reify
--
reify :: A.Expr -> C.WExpr
reify (A.Atom a) = withMeta [] (C.Atom a)
reify (A.Float a) = withMeta [] (C.Float a)
reify (A.Integer a) = withMeta [] (C.Integer a)
reify (A.List a) = withMeta [] (C.List $ reify <$> a)
reify (A.Keywords a) =
  withMeta
    []
    (C.List $ (\(x, y) -> withMeta [] (C.Tuple [reify x, reify y])) <$> a)
reify (A.String a) = withMeta [] (C.String a)
reify (A.Pair (a, b)) = withMeta [] (C.Tuple [reify a, reify b])
reify (A.Triple (A.Atom name, A.Keywords meta, A.Atom "Elixir")) =
  withMeta meta (C.Variable name)
reify (A.Triple (A.Atom name, A.Keywords meta, A.Atom "nil")) =
  withMeta meta (C.Variable name)
reify (A.Triple (A.Atom "__block__", A.Keywords meta, A.List exprs)) =
  withMeta meta (C.Block $ C.BlockVal $ reify <$> exprs)
reify (A.Triple (A.Atom "__aliases__", A.Keywords meta, A.List (a:aliases))) =
  withMeta meta (C.Alias (reify a, fmap raw aliases))
reify (A.Triple (A.Atom "{}", A.Keywords meta, A.List exprs)) =
  withMeta meta (C.Tuple $ reify <$> exprs)
reify (A.Triple (A.Atom "%{}", A.Keywords meta, A.List [])) =
  withMeta meta (C.Map [])
reify (A.Triple (A.Atom "%{}", A.Keywords meta, A.Keywords kv)) =
  withMeta meta (C.Map $ both reify <$> kv)
reify (A.Triple (A.Atom "<<>>", A.Keywords meta, A.List exprs)) =
  withMeta meta (C.Binary $ reify <$> exprs)
reify (A.Triple (A.Atom "fn", A.Keywords meta, A.List exprs)) =
  withMeta meta (C.Fn $ reify <$> exprs)
reify (A.Triple (A.Triple (A.Atom ".", A.Keywords _, A.List [lhs, A.Atom rhs]), A.Keywords meta, A.List args)) =
  withMeta meta (C.QualifiedCall (reify lhs) rhs (reify <$> args))
reify (A.Triple (A.Triple (A.Atom ".", A.Keywords _, A.List [expr]), A.Keywords meta, A.List args)) =
  withMeta meta (C.AnonymousCall (reify expr) (reify <$> args))
reify (A.Triple (A.Atom name, A.Keywords meta, A.List args)) =
  withMeta meta (C.NonQualifiedCall name (reify <$> args))
reify (A.Triple (A.Pair a, A.Keywords meta, A.List args)) =
  withMeta
    meta
    (C.Tuple [reify $ A.Pair a, withMeta [] (C.List $ reify <$> args)])
reify (A.Triple (A.Triple a, A.Keywords meta, A.List args)) =
  withMeta
    meta
    (C.Tuple [reify $ A.Triple a, withMeta [] (C.List $ reify <$> args)])
reify (A.Triple (A.Atom name, A.Keywords meta, A.Keywords args)) =
  withMeta meta (C.NonQualifiedCall name (reifyTuple <$> args))

--reify (A.Triple (A.Pair a, A.Keywords meta, A.Keywords args)) = C.Tuple [reify $ A.Pair a, C.List $ reify <$> meta, C.List $ reify <$> args]
--reify (A.Triple (A.Triple a, A.Keywords meta, A.Keywords args)) = C.Tuple [reify $ A.Triple a, C.List $ reify <$> meta, C.List $ reify <$> args]
reifyTuple :: (A.Expr, A.Expr) -> C.WExpr
reifyTuple (x, y) = withMeta [] (C.Tuple [reify x, reify y])

raw :: A.Expr -> String
raw (A.Atom a) = a

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

withMeta :: [(A.Expr, A.Expr)] -> C.Expr -> C.WExpr
withMeta m e = writer (e, metaOrEmpty m)

metaOrEmpty :: [(A.Expr, A.Expr)] -> M.Meta
metaOrEmpty ((A.Atom "line", A.Integer line):_) = M.Meta line
metaOrEmpty (_:xs) = metaOrEmpty xs
metaOrEmpty [] = M.Empty

parseAST :: String -> Either P.ParseError A.Expr
parseAST = parse P.parser ""

--
-- REPL
--
readAST :: String -> IO ()
readAST s =
  case parse P.parser "" s of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show (reify ast) ++ "\n"
