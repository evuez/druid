module Expr.Concrete where

import Meta (Meta)
import Control.Monad.Writer (Writer, tell)
import Data.List (intercalate)
import Data.Typeable (Typeable)

type WExpr = Writer Meta Expr

newtype BlockVal = BlockVal [WExpr] deriving (Show, Eq)

data Expr
  = Atom String
  | Alias (WExpr, [String])
  | Binary [WExpr]
--  | BinaryOp Operator
--             WExpr
--             WExpr
  | Block BlockVal
  | Charlist String
  | Float Float
  | Fn [WExpr]
  | Integer Integer
  | List [WExpr]
  | Map [(WExpr, WExpr)]
  | MapUpdate { expr :: WExpr
              , updates :: [(WExpr, WExpr)] }
  | NonQualifiedCall { name :: String
                     , args :: [WExpr] }
  | QualifiedCall { expr :: WExpr
                  , name :: String
                  , args :: [WExpr] }
  | AnonymousCall { expr :: WExpr
                  , args :: [WExpr] }
  | Sigil { ident :: Char
          , contents :: String
          , modifiers :: [Char] }
  | String String
  | Struct { alias :: WExpr
           , map :: [(WExpr, WExpr)] }
  | StructUpdate { alias :: WExpr
                 , expr :: WExpr
                 , updates :: [(WExpr, WExpr)] }
  | Tuple [WExpr]
--  | UnaryOp Operator
--            WExpr
  | Variable String
  | Module { alias :: WExpr, body :: BlockVal }
  | Def { name :: String, args :: [WExpr], body :: BlockVal }
  | DefP { name :: String, args :: [WExpr], body :: BlockVal }
  | DefMacro { name :: String, args :: [WExpr], body :: BlockVal }
  | Attribute { name :: String, expr :: WExpr }
  | Import { alias :: WExpr, opts :: [WExpr] }
  | Use { alias :: WExpr, opts :: [WExpr] }
  | Require { alias :: WExpr, opts :: [WExpr] } deriving (Show, Eq)
