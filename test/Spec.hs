module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Math

main = hspec $ do
  describe "processMathExpression" $ do
    it "Testing expression" $ do
      processMathExpression "(3+(4-1))*5" `shouldBe` "30.0"

    it "Testing linear equation" $ do
      processMathExpression "2 * x + 0.5 = 1" `shouldBe` "0.25"

    it "Testing linear equation" $ do
      processMathExpression "2x + 1 = 2(1-x)" `shouldBe` "0.25"
