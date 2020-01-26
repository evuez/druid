module Lib
  ( toBase
  , toConcrete
  , parseAST
  , readAST
  ) where

import qualified Expr.AST as A (Expr(..), reify)
import qualified Expr.Base as B (ExprW, reify)
import qualified Expr.Concrete as C (ExprW)
import qualified Parser as P (ParseError, parser)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

parseAST :: String -> Either P.ParseError A.Expr
parseAST = parse P.parser ""

toBase :: String -> Either P.ParseError B.ExprW
toBase s = fmap A.reify (parse P.parser "" s)

toConcrete :: String -> Either P.ParseError C.ExprW
toConcrete s = fmap B.reify (toBase s)

--
-- REPL
--
readAST :: String -> IO ()
readAST s =
  case parse P.parser "" s of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show (A.reify ast) ++ "\n"
