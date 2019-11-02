module Lib
  ( readAST
  ) where

import qualified Parser as P (parser)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

--
-- REPL
--
readAST :: String -> IO ()
readAST s =
  case parse P.parser "" s of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show ast ++ "\n"
