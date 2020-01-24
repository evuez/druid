module Expr.Base (Expr(..) , WExpr , Meta(..)) where

import Control.Monad.Writer (Writer, tell)
import Data.List (intercalate)
import Data.Typeable (Typeable)

data Meta
  = Meta { line :: Integer }
  | Empty
  deriving (Show, Eq)

type WExpr = Writer Meta Expr

instance Monoid Meta where
  mempty = Empty

instance Semigroup Meta where
  (<>) = const

data Expr
  = Atom String
  | Alias (WExpr, [String])
  | Binary [WExpr]
--  | BinaryOp Operator
--             WExpr
--             WExpr
  | Block [WExpr]
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
  deriving (Typeable, Eq)

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
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
-- showExpr (BinaryOp op a b) =
--   concat ["{:", showOp op, ", [], [", show a, ", ", show b, "]}"]
-- showExpr (UnaryOp op a) = concat ["{:", showOp op, ", [], [", show a, "]}"]
showExpr (Fn exprs) =
  concat ["{:fn, [], [", intercalate ", " $ show <$> exprs, "]}"]
