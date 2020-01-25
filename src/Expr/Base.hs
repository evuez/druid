module Expr.Base (BlockVal(BlockVal), Expr(..), ExprW) where

import Meta (MetaW, Meta(..))
import Utils (both)
import qualified Expr.Concrete as C (Expr(..), ExprW, BlockVal(..))
import Data.Bifunctor (second)
import Control.Monad.Writer (Writer, writer, runWriter)

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

reify :: Expr -> C.Expr
reify (Atom a) = (C.Atom a)
reify (Alias (a, b)) = (C.Alias (run a, b))
reify (Binary a) = (C.Binary $ run <$> a)
-- Block
reify (Charlist a) = C.Charlist a
reify (Float a) = C.Float a
reify (Fn a) = C.Fn $ run <$> a -- NOTE: Shouldn't that only be in Concrete?
reify (Integer a) = C.Integer a
reify (List a) = C.List $ run <$> a
reify (Map a) = C.Map $ both run <$> a
reify (MapUpdate a b) = C.MapUpdate (run a) (both run <$> b)
reify (NonQualifiedCall "defmodule" (x@(Alias _):xs)) = C.Module (run x) (C.BlockVal $ run <$> xs)

run :: ExprW -> C.ExprW
run exprAndMeta = do
  let (e, m) = runWriter exprAndMeta
  writer (reify e, m)
