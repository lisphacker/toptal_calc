{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
import Reflex
import Reflex.Dom

import Math
import Error

import Data.Text as T
import Data.FileEmbed
import Data.Map.Strict

import Control.Monad
import Control.Monad.Fix

noTxt = pack ""

data HistoryEntry = HistoryEntry { input :: Text
                                 , output :: Text
                                 } deriving (Show)
                    
main :: IO ()
main = mainWidgetWithCss $(embedFile "static/css/bootstrap.min.css") $ el "div" $  mathWeb

mathWeb :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
           => m ()
mathWeb = do
  elClass  "div" "" $ do
    rec ioEnteredEvent <- mathInput
        history <- foldDyn (:) [] ioEnteredEvent
        genHistoryElements history
        
        
    return ()

genHistoryElement :: ( DomBuilder t m
                     , DomBuilderSpace m ~ GhcjsDomSpace
                     , MonadFix m
                     , MonadHold t m
                     , PostBuild t m
                     )
                     => Dynamic t HistoryEntry -> m ()
genHistoryElement histDyn = el "div" $ do
  elClass "table" "table table-bordered" $ do
    el "tbody" $ do
      el "tr" $ do
        elClass "th" "col-sm-2" $ text "Input"
        elClass "td" "col-sm-10" $ dynText $ fmap input histDyn
      el "tr" $ do
        elClass "th" "col-sm-2" $ text "Output"
        elClass "td" "col-sm-10" $ dynText $ fmap output histDyn
  return ()

genHistoryElements :: ( DomBuilder t m
                      , DomBuilderSpace m ~ GhcjsDomSpace
                      , MonadFix m
                      , MonadHold t m
                      , PostBuild t m
                      )
                      => Dynamic t [HistoryEntry] -> m ()
genHistoryElements history = do
  simpleList history genHistoryElement
  return ()
                                

keyCodeIs :: Key -> KeyCode -> Bool
keyCodeIs k c = keyCodeLookup c == k

mathInput :: ( DomBuilder t m
             , DomBuilderSpace m ~ GhcjsDomSpace
             , MonadFix m
             , MonadHold t m
             , PostBuild t m
             )
             => m (Event t HistoryEntry)
mathInput = do
  rec mathInput <- elClass "div" "input-group" $ do
        elClass "span" "input-group-addon" $ text "Input"
        textInput $ def
          & textInputConfig_attributes .~ (constDyn $ fromList [("class", "form-control")])
          & textInputConfig_setValue .~ fmap (const "") newValueEntered
      let newValueEntered = ffilter (keyCodeIs Enter . fromIntegral) (_textInput_keypress mathInput)
        
  let (errDynText, resultDynText) = splitDynPure  $ fmap runMath $ _textInput_value mathInput
  elClass "p" "text-danger" $ dynText $ prependPrefix "Error: " errDynText
  el "p" $ dynText $ prependPrefix "Preview: " resultDynText

  let ioDyn = zipDynWith (\i o -> (i, o)) (_textInput_value mathInput) resultDynText
      io = tag (current ioDyn) newValueEntered
  return $ fmapMaybe validateIO io
    where validateIO (i, o) = let i' = strip i
                                  o' = strip o
                              in if T.null i' || T.null o' then Nothing else Just $ HistoryEntry i' o'
                                   
          prependPrefix prefix = fmap (\s -> if T.null s then noTxt else pack $ prefix ++ unpack s)


runMath :: Text -> (Text, Text)
runMath input = case processMathExpression (unpack input) of
                 Left (MathError ec) -> (if ec == ErrEmptyInput then noTxt else pack (show ec), noTxt)
                 Right v              -> (noTxt, pack v')
                   where v' = if Prelude.length v == 1 then (show . Prelude.head) v else show v

