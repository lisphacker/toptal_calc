module Main where

import Math
import System.IO

prompt = "> "

main :: IO ()
main = do
  putStr prompt
  hFlush stdout
  input <- getLine
  putStrLn $ processMathExpression input
  main
