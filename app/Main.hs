module Main where

import Lib (readAST)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= readAST . head
