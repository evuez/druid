module Expr.Concrete (BlockVal(BlockVal), Expr(..), ExprW) where

import Meta (MetaW)

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
  | Variable String
  | Module { alias :: ExprW, body :: BlockVal }
  | Def { name :: String, args :: [ExprW], body :: BlockVal }
  | DefP { name :: String, args :: [ExprW], body :: BlockVal }
  | DefMacro { name :: String, args :: [ExprW], body :: BlockVal }
  | Attribute { name :: String, expr :: ExprW }
  | Import { alias :: ExprW, opts :: [ExprW] }
  | Use { alias :: ExprW, opts :: [ExprW] }
  | Require { alias :: ExprW, opts :: [ExprW] } deriving (Show, Eq)
