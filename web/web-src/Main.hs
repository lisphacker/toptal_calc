{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
import Reflex.Dom
import Math
import Data.Text
import Data.FileEmbed
import Data.Map.Strict

main :: IO ()
main = mainWidgetWithCss $(embedFile "static/css/bootstrap.min.css") $ el "div" $ do
  t <- elClass "div" "input-group" $ do
    elClass "span" "input-group-addon" $ text "Input"
    textInputWithClass "form-control"
  let (errDynText, resultDynText) = splitDynPure  $ fmap runMath $ _textInput_value t
  elClass "p" "text-danger" $ dynText errDynText
  el "p" $ dynText resultDynText

textInputWithClass cls = let cfg = constDyn $ fromList [("class", cls)]
                         in textInput $ def & textInputConfig_attributes .~ cfg
                            

noTxt = pack ""
runMath :: Text -> (Text, Text)
runMath input = case processMathExpression (unpack input) of
                 Left (MathError msg) -> (pack $ if msg == "Parse error: Empty input" then "" else msg, noTxt)
                 Right v              -> (noTxt, pack $ v')
                   where v' = if Prelude.length v == 1 then (show . Prelude.head) v else show v

