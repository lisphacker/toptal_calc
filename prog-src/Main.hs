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

runMath :: String -> String
runMath input = case processMathExpression input of
  Left (MathError msg) -> "*** " ++ msg ++ " ***"
  Right v -> if length v == 1 then
               show $ head v
             else
               show v
               
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      mInput <- getInputLine prompt
      case mInput of
        Just "exit" -> return()
        Just input  -> do outputStrLn $ runMath input
                          loop
        Nothing     -> loop
