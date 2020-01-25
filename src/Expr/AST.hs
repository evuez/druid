module Expr.AST (Expr(..), reify) where

import Control.Monad.Writer (writer)
import Data.List (intercalate)
import qualified Meta as M (Meta(..))
import qualified Expr.Base as B (BlockVal(..), Expr(..), WExpr)
import Utils (both)

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


reify :: Expr -> B.WExpr
reify (Atom a) = withMeta [] (B.Atom a)
reify (Float a) = withMeta [] (B.Float a)
reify (Integer a) = withMeta [] (B.Integer a)
reify (List a) = withMeta [] (B.List $ reify <$> a)
reify (Keywords a) =
  withMeta
    []
    (B.List $ (\(x, y) -> withMeta [] (B.Tuple [reify x, reify y])) <$> a)
reify (String a) = withMeta [] (B.String a)
reify (Pair (a, b)) = withMeta [] (B.Tuple [reify a, reify b])
reify (Triple (Atom name, Keywords meta, Atom "Elixir")) =
  withMeta meta (B.Variable name)
reify (Triple (Atom name, Keywords meta, Atom "nil")) =
  withMeta meta (B.Variable name)
reify (Triple (Atom "__block__", Keywords meta, List exprs)) =
  withMeta meta (B.Block $ B.BlockVal $ reify <$> exprs)
reify (Triple (Atom "__aliases__", Keywords meta, List (a:aliases))) =
  withMeta meta (B.Alias (reify a, fmap raw aliases))
reify (Triple (Atom "{}", Keywords meta, List exprs)) =
  withMeta meta (B.Tuple $ reify <$> exprs)
reify (Triple (Atom "%{}", Keywords meta, List [])) =
  withMeta meta (B.Map [])
reify (Triple (Atom "%{}", Keywords meta, Keywords kv)) =
  withMeta meta (B.Map $ both reify <$> kv)
reify (Triple (Atom "<<>>", Keywords meta, List exprs)) =
  withMeta meta (B.Binary $ reify <$> exprs)
reify (Triple (Atom "fn", Keywords meta, List exprs)) =
  withMeta meta (B.Fn $ reify <$> exprs)
reify (Triple (Triple (Atom ".", Keywords _, List [lhs, Atom rhs]), Keywords meta, List args)) =
  withMeta meta (B.QualifiedCall (reify lhs) rhs (reify <$> args))
reify (Triple (Triple (Atom ".", Keywords _, List [expr]), Keywords meta, List args)) =
  withMeta meta (B.AnonymousCall (reify expr) (reify <$> args))
reify (Triple (Atom name, Keywords meta, List args)) =
  withMeta meta (B.NonQualifiedCall name (reify <$> args))
reify (Triple (Pair a, Keywords meta, List args)) =
  withMeta
    meta
    (B.Tuple [reify $ Pair a, withMeta [] (B.List $ reify <$> args)])
reify (Triple (Triple a, Keywords meta, List args)) =
  withMeta
    meta
    (B.Tuple [reify $ Triple a, withMeta [] (B.List $ reify <$> args)])
reify (Triple (Atom name, Keywords meta, Keywords args)) =
  withMeta meta (B.NonQualifiedCall name (reifyTuple <$> args))
reify x = error ("Invalid AST: " ++ show x)

--reify (Triple (Pair a, Keywords meta, Keywords args)) = B.Tuple [reify $ Pair a, B.List $ reify <$> meta, B.List $ reify <$> args]
--reify (Triple (Triple a, Keywords meta, Keywords args)) = B.Tuple [reify $ Triple a, B.List $ reify <$> meta, B.List $ reify <$> args]
reifyTuple :: (Expr, Expr) -> B.WExpr
reifyTuple (x, y) = withMeta [] (B.Tuple [reify x, reify y])

raw :: Expr -> String
raw (Atom a) = a
raw x = error ("Not an atom: " ++ show x)

withMeta :: [(Expr, Expr)] -> B.Expr -> B.WExpr
withMeta m e = writer (e, metaOrEmpty m)

metaOrEmpty :: [(Expr, Expr)] -> M.Meta
metaOrEmpty ((Atom "line", Integer line):_) = M.Meta line []
metaOrEmpty (_:xs) = metaOrEmpty xs
metaOrEmpty [] = M.Empty
