module Expr
  ( EExpr(..)
  , AExpr(..)
  , Operator(..)
  ) where

import Data.List (intercalate)
import Data.Typeable (Typeable)

data EExpr
  = Atom String
  | Alias (EExpr, [String])
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
  | QualifiedCall { expr :: EExpr
                  , name :: String
                  , args :: [EExpr] }
  | AnonymousCall { expr :: EExpr
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

data AExpr
  = APair (AExpr, AExpr)
  | ATriple (AExpr, AExpr, AExpr)
  | AKeywords [(AExpr, AExpr)]
  | AList [AExpr]
  | AAtom String
  | AInteger Integer
  | AFloat Float
  | AString String
  | ACharlist String

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

instance Show AExpr where
  show = showAExpr

instance Show EExpr where
  show = showExpr

showExpr :: EExpr -> String
showExpr (Atom atom) = concat [":", atom]
showExpr (Alias (expr, aliases)) =
  concat
    ["{:__aliases__, [], [", show expr, ", :", intercalate ", :" aliases, "]}"]
showExpr (Block exprs) =
  concat ["{:__block__, [], [", intercalate ", " $ show <$> exprs, "]}"]
showExpr (Integer integer) = show integer
showExpr (Float float) = show float
showExpr (String text) = concat ["\"", text, "\""]
showExpr (Charlist text) = concat ["'", text, "'"]
showExpr (Variable name') = concat ["{:", name', ", [], Elixir}"]
showExpr (Tuple [expr1, expr2]) =
  concat ["{", show expr1, ", ", show expr2, "}"]
showExpr (Tuple exprs) =
  concat ["{:{}, [], [", intercalate ", " $ show <$> exprs, "]}"]
showExpr (Binary exprs) =
  concat ["{:<<>>, [], [", intercalate ", " $ show <$> exprs, "]}"]
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
showExpr (List exprs) = concat ["[", intercalate ", " $ show <$> exprs, "]"]
showExpr (Map keyValues) =
  concat
    [ "%{"
    , intercalate ", " $
      (\(k, v) -> concat [show k, " => ", show v]) <$> keyValues
    , "}"
    ]
showExpr (MapUpdate expr' updates') =
  concat
    [ "{:%{}, [], [{:|, [], ["
    , show expr'
    , ", ["
    , intercalate ", " $
      (\(k, v) -> concat ["{", show k, ", ", show v, "}"]) <$> updates'
    , "]]}]}"
    ]
showExpr (Struct alias' keyValues) =
  concat
    [ "%"
    , show alias'
    , "{"
    , intercalate ", " $
      (\(k, v) -> concat [show k, " => ", show v]) <$> keyValues
    , "}"
    ]
showExpr (StructUpdate alias' expr' updates') =
  concat
    [ "{:%, [], [{:__aliases__, [], [:"
    , show alias'
    , "]}, {:%{}, [], [{:|, [], ["
    , show expr'
    , ", ["
    , intercalate ", " $
      (\(k, v) -> concat ["{", show k, ", ", show v, "}"]) <$> updates'
    , "]]}]}]}"
    ]
showExpr (QualifiedCall expr' name' args') =
  concat
    [ "QualifiedCall<{{:., [], ["
    , show expr'
    , ", :"
    , name'
    , "]}, [], ["
    , intercalate ", " $ show <$> args'
    , "]}>"
    ]
showExpr (NonQualifiedCall name' args') =
  concat
    [ "NonQualifiedCall<{:"
    , name'
    , ", [], ["
    , intercalate ", " $ show <$> args'
    , "]}>"
    ]
showExpr (AnonymousCall expr' args') =
  concat
    [ "AnonymousCall<{{:., [], [{:"
    , show expr'
    , ", [], Elixir}]}, [], ["
    , intercalate ", " $ show <$> args'
    , "]}>"
    ]
showExpr (BinaryOp op a b) =
  concat ["{:", showOp op, ", [], [", show a, ", ", show b, "]}"]
showExpr (UnaryOp op a) = concat ["{:", showOp op, ", [], [", show a, "]}"]
showExpr (Fn exprs) =
  concat ["{:fn, [], [", intercalate ", " $ show <$> exprs, "]}"]

showAExpr :: AExpr -> String
showAExpr (APair (a, b)) = concat ["{", show a, ", ", show b, "}"]
showAExpr (ATriple (a, b, c)) =
  concat ["{:{}, [], [", show a, ", ", show b, ", ", show c, "]}"]
showAExpr (AKeywords _) = "keywords"
showAExpr (AList a) = show a
showAExpr (AAtom a) = concat [":", a]
showAExpr (AInteger a) = show a
showAExpr (AFloat a) = show a
showAExpr (AString a) = a
showAExpr (ACharlist a) = a

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
