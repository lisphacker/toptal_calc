{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import Math
import Data.Text

main = mainWidget $ el "div" $ do
  t <- textInput def
  dynText $ fmap (pack . processMathExpression . unpack) $ _textInput_value t
