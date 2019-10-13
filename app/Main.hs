module Main where

import Lib (readSource)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= readSource . head
