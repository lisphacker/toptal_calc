module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad

import Error
import Math

expressionTests =  [ ("(3+(4-1))*5",[30.0])
                   , ("3--(---2)",[1.0])
                   , ("4*-(3)",[-12.0])
                   , ("((12)-(-(-(3))))", [9.0])
                   , ("1+sin -deg2rad -30", [1.5])
                   ]

linEqnTests = [ ("2 * x + 0.5 = 1",[0.25])
              , ("2x + 1 = 2(1-x)",[0.25])
              , ("30.0 + x=0",[-30.0])
              ]

quadEqnTests = [ ("xx-1=0",[-1.0, 1.0])
               , ("2xx - 11x + 15=0",[2.5, 3.0])
               , ("(x-3)(x-2)=0",[3.0, 2.0])
               , ("x(x-5)=0",[0.0,5.0])
               ]

unexpCharTests = [ ("a%10=0", ErrUnexpectedCharInInputAt "%10=0")
                 ]

runMathWithVal expr = case processMathExpression expr of
  Left (MathError ec) -> error $ show ec
  Right val           -> val

runMathWithErr expr = case processMathExpression expr of
  Left (MathError ec) -> ec
  Right val           -> error "Should have failed"

main = hspec $ do
  describe "processMathExpression" $ do
    
    it "Testing expression evaluation" $ do
      forM_ expressionTests testEqualValues
              
    it "Testing solving linear equations" $ do
      forM_ linEqnTests testEqualValues

    it "Testing solving quadratic equations" $ do
      forM_ quadEqnTests testEqualLists
      
    it "Testing unexpected characters in input" $ do
      forM_ unexpCharTests testError

        where testEqualValues (expr, result) = runMathWithVal expr `shouldBe` result
              testEqualLists (expr, result) = runMathWithVal expr `shouldMatchList` result
              testError (expr, result) = runMathWithErr expr `shouldBe` result
