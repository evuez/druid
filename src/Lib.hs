module Lib
  ( decode
  , parseAST
  , readAST
  ) where

import qualified Expr.AST as A (Expr(..), reify)
import qualified Expr.Base as B (BlockVal(..), Expr(..), WExpr)
import qualified Meta as M (Meta(..))
import qualified Parser as P (ParseError, parser)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

parseAST :: String -> Either P.ParseError A.Expr
parseAST = parse P.parser ""

decode :: String -> Either P.ParseError B.WExpr
decode s = fmap A.reify (parse P.parser "" s)

--
-- REPL
--
readAST :: String -> IO ()
readAST s =
  case parse P.parser "" s of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show (A.reify ast) ++ "\n"
