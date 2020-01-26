module Expr.Base (BlockVal(BlockVal), Expr(..), ExprW, reify) where

import Meta (MetaW)
import Utils (both)
import qualified Expr.Concrete as C (Expr(..), ExprW, BlockVal(..))
import Control.Monad.Writer (runWriter)

type ExprW = MetaW Expr

newtype BlockVal = BlockVal [ExprW] deriving (Show, Eq)

data Expr
  = Atom String
  | Alias (ExprW, [String])
  | Binary [ExprW]
--  | BinaryOp Operator
--             ExprW
--             ExprW
  | Block BlockVal
  | Charlist String
  | Float Float
  | Fn [ExprW]
  | Integer Integer
  | List [ExprW]
  | Map [(ExprW, ExprW)]
  | MapUpdate { expr :: ExprW
              , updates :: [(ExprW, ExprW)] }
  | NonQualifiedCall { name :: String
                     , args :: [ExprW] }
  | QualifiedCall { expr :: ExprW
                  , name :: String
                  , args :: [ExprW] }
  | AnonymousCall { expr :: ExprW
                  , args :: [ExprW] }
  | Sigil { ident :: Char
          , contents :: String
          , modifiers :: [Char] }
  | String String
  | Struct { alias :: ExprW
           , map :: [(ExprW, ExprW)] }
  | StructUpdate { alias :: ExprW
                 , expr :: ExprW
                 , updates :: [(ExprW, ExprW)] }
  | Tuple [ExprW]
--  | UnaryOp Operator
--            ExprW
  | Variable String deriving (Show, Eq)

reify :: ExprW -> C.ExprW
reify exprAndMeta = fmap doReify exprAndMeta

doReify :: Expr -> C.Expr
doReify (Atom a) = (C.Atom a)
doReify (Alias (a, b)) = (C.Alias (reify a, b))
doReify (Binary a) = (C.Binary $ reify <$> a)
doReify (Block (BlockVal a)) = C.Block (C.BlockVal $ reify <$> a)
doReify (Charlist a) = C.Charlist a
doReify (Float a) = C.Float a
doReify (Fn a) = C.Fn $ reify <$> a -- NOTE: Shouldn't that only be in Concrete?
doReify (Integer a) = C.Integer a
doReify (List a) = C.List $ reify <$> a
doReify (Map a) = C.Map $ both reify <$> a
doReify (MapUpdate a b) = C.MapUpdate (reify a) (both reify <$> b)
doReify (NonQualifiedCall a b@(x:xs)) =
  case (a, runWriter x) of
    ("defmodule", ((Alias _), _)) -> C.Module (reify x) (C.BlockVal $ reify <$> xs)
    ("defmodule", _) -> error "Invalid module definition." -- NonQualifiedCall "defmodule" ... + errors ["Invalid module def"]?
    ("def", ((Variable name'), _)) -> C.Def name' [] (C.BlockVal $ reify <$> xs)
    ("def", ((NonQualifiedCall name' args'), _)) -> C.Def name' (reify <$> args') (C.BlockVal $ reify <$> xs)
    ("def", _) -> error "Invalid function definition."
    ("defp", ((Variable name'), _)) -> C.DefP name' [] (C.BlockVal $ reify <$> xs)
    ("defp", ((NonQualifiedCall name' args'), _)) -> C.DefP name' (reify <$> args') (C.BlockVal $ reify <$> xs)
    ("defp", _) -> error "Invalid private function definition."
    ("defmacro", ((Variable name'), _)) -> C.DefMacro name' [] (C.BlockVal $ reify <$> xs)
    ("defmacro", ((NonQualifiedCall name' args'), _)) -> C.DefMacro name' (reify <$> args') (C.BlockVal $ reify <$> xs)
    ("defmacro", _) -> error "Invalid macro definition."
    _ -> C.NonQualifiedCall a (reify <$> b)
doReify (NonQualifiedCall a b) = C.NonQualifiedCall a (reify <$> b)
doReify (QualifiedCall a b c) = C.QualifiedCall (reify a) b (reify <$> c)
doReify (AnonymousCall a b) = C.AnonymousCall (reify a) (reify <$> b)
doReify (Sigil a b c) = C.Sigil a b c
doReify (String a) = C.String a
doReify (Struct a b) = C.Struct (reify a) (both reify <$> b)
doReify (StructUpdate a b c) = C.StructUpdate (reify a) (reify b) (both reify <$> c)
doReify (Tuple a) = C.Tuple (reify <$> a)
doReify (Variable a) = C.Variable a
