module Expr
  ( EExpr(..)
  , Operator(..)
  ) where

import Data.List (intercalate)
import Data.Typeable (Typeable)

data EExpr
  = Atom String
  | Alias [String] -- Alias EExpr String
  | Binary [EExpr]
  | BinaryOp Operator
             EExpr
             EExpr
  | Block [EExpr]
  | Charlist String
  | Float Float
  | Fn [EExpr]
  | Integer Integer
  | List [EExpr]
  | Map [(EExpr, EExpr)]
  | MapUpdate { expr :: EExpr
              , updates :: [(EExpr, EExpr)] }
  | NonQualifiedCall { name :: String
                     , args :: [EExpr] }
  | QualifiedCall { alias :: EExpr
                  , name :: String
                  , args :: [EExpr] }
  | Sigil { ident :: Char
          , contents :: String
          , modifiers :: [Char] }
  | String String
  | Struct { alias :: EExpr
           , map :: [(EExpr, EExpr)] }
  | StructUpdate { alias :: EExpr
                 , expr :: EExpr
                 , updates :: [(EExpr, EExpr)] }
  | Tuple [EExpr]
  | UnaryOp Operator
            EExpr
  | Variable String
  deriving (Typeable, Eq)

data Operator
  = And
  | Application
  | Assignment
  | Attribute
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
  deriving (Eq)

instance Show EExpr where
  show = showExpr

-- TODO: Display ast form instead
showExpr :: EExpr -> String
showExpr (Atom atom) = concat [":", atom]
showExpr (Alias alias') =
  concat ["{:__aliases__, [], [:", intercalate ", :" alias', "]}"]
showExpr (Block exprs) =
  concat ["{:__block__, [], [", intercalate ", " $ showExpr <$> exprs, "]}"]
showExpr (Integer integer) = show integer
showExpr (Float float) = show float
showExpr (String text) = concat ["\"", text, "\""]
showExpr (Charlist text) = concat ["'", text, "'"]
showExpr (Variable name') = concat ["{:", name', ", [], Elixir}"]
showExpr (Tuple [expr1, expr2]) =
  concat ["{", showExpr expr1, ", ", showExpr expr2, "}"]
showExpr (Tuple exprs) =
  concat ["{:{}, [], [", intercalate ", " $ showExpr <$> exprs, "]}"]
showExpr (Binary exprs) =
  concat ["{:<<>>, [], [", intercalate ", " $ showExpr <$> exprs, "]}"]
showExpr (Sigil ident' contents' modifiers') =
  concat
    [ "{:sigil_"
    , [ident']
    , ", [], [{:<<>>, [], [\""
    , contents'
    , "\"]}, '"
    , modifiers'
    , "']}"
    ]
showExpr (List exprs) = concat ["[", intercalate ", " $ showExpr <$> exprs, "]"]
showExpr (Map keyValues) =
  concat
    [ "%{"
    , intercalate ", " $
      (\(k, v) -> concat [showExpr k, " => ", showExpr v]) <$> keyValues
    , "}"
    ]
showExpr (MapUpdate expr' updates') =
  concat
    [ "{:%{}, [], [{:|, [], ["
    , showExpr expr'
    , ", ["
    , intercalate ", " $
      (\(k, v) -> concat ["{", showExpr k, ", ", showExpr v, "}"]) <$> updates'
    , "]]}]}"
    ]
showExpr (Struct alias' keyValues) =
  concat
    [ "%"
    , showExpr alias'
    , "{"
    , intercalate ", " $
      (\(k, v) -> concat [showExpr k, " => ", showExpr v]) <$> keyValues
    , "}"
    ]
showExpr (StructUpdate alias' expr' updates') =
  concat
    [ "{:%, [], [{:__aliases__, [], [:"
    , showExpr alias'
    , "]}, {:%{}, [], [{:|, [], ["
    , showExpr expr'
    , ", ["
    , intercalate ", " $
      (\(k, v) -> concat ["{", showExpr k, ", ", showExpr v, "}"]) <$> updates'
    , "]]}]}]}"
    ]
showExpr (QualifiedCall alias' name' args') =
  concat
    [ "{:., [], ["
    , showExpr alias'
    , ", :"
    , name'
    , "]}, [], ["
    , intercalate ", " $ showExpr <$> args'
    , "]}"
    ]
showExpr (NonQualifiedCall name' args') =
  concat ["{:", name', ", [], [", intercalate ", " $ showExpr <$> args', "]}"]
showExpr (BinaryOp op a b) =
  concat ["{:", showOp op, ", [], [", showExpr a, ", ", showExpr b, "]}"]
showExpr (UnaryOp op a) = concat ["{:", showOp op, ", [], [", showExpr a, "]}"]
showExpr (Fn exprs) =
  concat ["{:fn, [], [", intercalate ", " $ showExpr <$> exprs, "]}"]

showOp :: Operator -> String
showOp And = "&&"
showOp Application = "."
showOp Assignment = "="
showOp Attribute = "@"
showOp Bang = "!"
showOp BitwiseAnd = "&&&"
showOp BitwiseNot = "~~~"
showOp BitwiseOr = "|||"
showOp BitwiseXor = "^^^"
showOp BooleanAnd = "and"
showOp BooleanOr = "or"
showOp Capture = "&"
showOp ChevronPipeChevron = "<|>"
showOp ChevronTilde = "<~"
showOp ChevronTildeChevron = "<~>"
showOp Concat = "++"
showOp DefaultArg = "\\"
showOp Difference = "--"
showOp Division = "/"
showOp DoubleChevronTilde = "<<~"
showOp Equal = "=="
showOp GreaterThan = ">"
showOp GreaterThanOrEqual = ">="
showOp Id = "+"
showOp In = "in"
showOp LeftArrow = "<-"
showOp LessThan = "<"
showOp LessThanOrEqual = "<="
showOp Negation = "-"
showOp Not = "not"
showOp NotEqual = "!="
showOp NotIn = "not in"
showOp Or = "||"
showOp Pin = "^"
showOp Pipe = "|"
showOp PipeRight = "|>"
showOp Product = "*"
showOp Range = ".."
showOp RegexEqual = "=~"
showOp RightArrow = "->"
showOp ShiftLeft = "<<<"
showOp ShiftRight = ">>>"
showOp SpecType = "::"
showOp StrictEqual = "==="
showOp StrictNotEqual = "!=="
showOp StringConcat = "<>"
showOp Subtraction = "-"
showOp Sum = "+"
showOp TildeChevron = "~>"
showOp TildeDoubleChevron = "~>>"
showOp When = "when"
