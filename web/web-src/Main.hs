{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import Math
import Data.Text

main = mainWidget $ el "div" $ do
  t <- textInput def
  let (errDynText, resultDynText) = splitDynPure  $ fmap runMath $ _textInput_value t
  el "p" $ dynText errDynText
  el "p" $ dynText resultDynText





--main :: IO ()
--main = mainWidgetWithCss $(embedFile "style.css") todoMVC

noTxt = pack ""
runMath :: Text -> (Text, Text)
runMath input = case processMathExpression (unpack input) of
                 Left (MathError msg) -> (pack $ if msg == "Parse error: Empty input" then "" else msg, noTxt)
                 Right v              -> (noTxt, pack $ v')
                   where v' = if Prelude.length v == 1 then (show . Prelude.head) v else show v
