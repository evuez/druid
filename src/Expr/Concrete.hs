module Expr.Concrete (BlockVal(BlockVal), Expr(..), ExprW, Operator(..), InnerClause(..)) where

import Meta (MetaW)

type ExprW = MetaW Expr

newtype BlockVal = BlockVal [ExprW] deriving (Show, Eq)

data Expr
  = Atom String
  | Alias (ExprW, [String])
  | Binary [ExprW]
  | BinaryOp Operator
             ExprW
             ExprW
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
  | Module { alias :: ExprW, body :: [InnerClause] }
  | Def { name :: String, args :: [ExprW], body :: [InnerClause] }
  | DefP { name :: String, args :: [ExprW], body :: [InnerClause] }
  | DefMacro { name :: String, args :: [ExprW], body :: [InnerClause] }
  | Attribute { name :: String, expr :: ExprW }
  | Import { alias :: ExprW, opts :: [ExprW] }
  | Use { alias :: ExprW, opts :: [ExprW] }
  | Require { alias :: ExprW, opts :: [ExprW] }
  deriving (Show, Eq)

data InnerClause
  = Do BlockVal
  | Else BlockVal
  | Rescue BlockVal
  | Catch BlockVal
  | After BlockVal
  deriving (Show, Eq)

data Operator
  = And
  | Application
  | Assignment
--  | Attribute
  | Bang
  | BitwiseAnd
  | BitwiseNot
  | BitwiseOr
  | BitwiseXor
  | BooleanAnd
  | BooleanOr
  | Capture
  | ChevronPipeChevron
  | ChevronTilde
  | ChevronTildeChevron
  | Concat
  | DefaultArg
  | Difference
  | Division
  | DoubleChevronTilde
  | Equal
  | GreaterThan
  | GreaterThanOrEqual
  | Id
  | In
  | LeftArrow
  | LessThan
  | LessThanOrEqual
  | Negation
  | Not
  | NotEqual
  | NotIn
  | Or
  | Pin
  | Pipe
  | PipeRight
  | Product
  | Range
  | RegexEqual
  | RightArrow
  | ShiftLeft
  | ShiftRight
  | SpecType
  | StrictEqual
  | StrictNotEqual
  | StringConcat
  | Subtraction
  | Sum
  | TildeChevron
  | TildeDoubleChevron
  | When
  deriving (Show, Eq)
