module Main where

import Math
import System.IO
import System.Console.Haskeline

prompt = "> "

main :: IO ()
{-
main = do
  input <- getInputLine prompt
  putStrLn $ processMathExpression input
  main
-}
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      mInput <- getInputLine prompt
      case mInput of
        Just "exit" -> return()
        Just input  -> do outputStrLn $ processMathExpression input
                          loop
        Nothing     -> loop
        
