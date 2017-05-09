module Main where

import Math
import Error
import System.IO
import System.Console.Haskeline

prompt = "> "


runMath :: String -> String
runMath input = case processMathExpression input of
  Left (MathError ec) -> "*** " ++ show ec ++ " ***"
  Right v -> if length v == 1 then
               show $ head v
             else
               show v

-- | Program entry point
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      mInput <- getInputLine prompt
      case mInput of
        Just "exit" -> return()
        Just "quit" -> return()
        Just input  -> do outputStrLn $ runMath input
                          loop
        Nothing     -> loop
