{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Eval
import Style

import Control.Monad
import Control.Monad.Fix (MonadFix)
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Text.Read (readMaybe)

data CalcPad
  = PadNum Int
  | PadOp Text
  | PadDot
  | PadBack
  | PadClear
  | PadEq


main :: IO ()
main = mainWidgetWithCss calcStyle $ mdo
  padEv <- padWidget screenDyn
  screenDyn <- foldDyn processPadInput ("0", mempty) padEv
  return ()

padWidget :: (PostBuild t m, DomBuilder t m) => Dynamic t (Text, Map Text Text) -> m (Event t CalcPad)
padWidget screenDyn =
  elClass "table" "calc-body" $ do
    el "tr" $ do
      let screenText = fst <$> screenDyn
          screenStyle = snd <$> screenDyn
          screenAttr = mappend <$> pure ("colspan" =: "4" <> "class" =: "calc-screen") <*> screenStyle
      elDynAttr "td" screenAttr $ dynText screenText

    row1 <- el "tr" $ do
      btnClear <- el "td" $ buttonClass PadClear "calc-item clear" "C"
      btnBack <- el "td" $ buttonClass PadBack "calc-item back" "←"
      btnMod <- el "td" $ calcButton (PadOp "%") "%"
      btnAdd <- el "td" $ calcButton (PadOp "+") "+"
      return [btnClear, btnBack, btnMod, btnAdd]
    
    row2 <- el "tr" $ do
      btn7 <- el "td" $ numButton (PadNum 7) "7"
      btn8 <- el "td" $ numButton (PadNum 8) "8"
      btn9 <- el "td" $ numButton (PadNum 9) "9"
      btnSub <- el "td" $ calcButton (PadOp "-") "-"
      return [btn7, btn8, btn9, btnSub]

    row3 <- el "tr" $ do
      btn4 <- el "td" $ numButton (PadNum 4) "4"
      btn5 <- el "td" $ numButton (PadNum 5) "5"
      btn6 <- el "td" $ numButton (PadNum 6) "6"
      btnMul <- el "td" $ calcButton (PadOp "×") "×"
      return [btn4, btn5, btn6, btnMul]
    
    row4 <- el "tr" $ do
      btn1 <- el "td" $ numButton (PadNum 1) "1"
      btn2 <- el "td" $ numButton (PadNum 2) "2"
      btn3 <- el "td" $ numButton (PadNum 3) "3"
      btnDiv <- el "td" $ calcButton (PadOp "÷") "÷"
      return [btn1, btn2, btn3, btnDiv]
    
    row5 <- el "tr" $ do
      btnDot <- el "td" $ calcButton PadDot "."
      btn0 <- el "td" $ numButton (PadNum 0) "0"
      btnEq <- elAttr "td" ("colspan" =: "2") (calcButton PadEq "=")
      return [btnDot, btn0, btnEq]

    let padEv = (leftmost . mconcat) [row1, row2, row3, row4, row5]
    return padEv
  

processPadInput :: CalcPad -> (Text, Map Text Text) -> (Text, Map Text Text)
processPadInput pad (input, _) = styleError input $ updateInputBox pad input

updateInputBox :: CalcPad -> Text -> Maybe Text
updateInputBox padOp "0" = updateInputBox padOp mempty 
updateInputBox padOp input
  | Text.length input == 10 && opAppends padOp = Just input
  | otherwise = case padOp of
      PadNum i ->
        Just $ input <> (pack . show) i

      PadOp arithOp -> Just $ input <> arithOp
      PadDot -> Just $ input <> "."

      PadBack | Text.length input <= 1 -> Just "0"
              | otherwise              -> Just $ Text.init input
      
      PadClear -> Just "0"
      PadEq -> formatDouble <$> eval input


-- Need this because I haven't figured out floating text boxes
-- This will give wrong answers when doubles have >10 digits
-- before the decimal.
formatDouble :: Double -> Text
formatDouble = Text.take 10 . dropTrailingZero . pack . show
  where dropTrailingZero t =
          if Text.length t >= 3 && Text.takeEnd 2 t == ".0"
            then Text.dropEnd 2 t
            else t

styleError :: Text -> Maybe Text -> (Text, Map Text Text)
styleError _ (Just result) = (result, mempty)
styleError input Nothing = (input, errStyle)
  where errStyle = "style" =: ("color: " <> "red")


opAppends :: CalcPad -> Bool
opAppends = \case
  PadNum _ -> True
  PadOp _  -> True
  PadDot   -> True
  PadBack  -> False
  PadClear -> False
  PadEq    -> False

buttonAttr :: (DomBuilder t m) => a -> Map Text Text -> Text -> m (Event t a)
buttonAttr a attrs label = do
  (btnEqDom, _) <- elAttr' "button" attrs $ text label
  return (a <$ domEvent Click btnEqDom)

calcButton :: (DomBuilder t m) => a -> Text -> m (Event t a)
calcButton a btnLabel = buttonClass a "calc-item" btnLabel

numButton :: (DomBuilder t m) => a -> Text -> m (Event t a)
numButton a btnLabel = buttonClass a "calc-item num" btnLabel

buttonClass :: (DomBuilder t m) => a -> Text -> Text -> m (Event t a)
buttonClass a elemTag btnLabel = do
  (btnEqDom, _) <- elClass' "button" elemTag $ text btnLabel
  return (a <$ domEvent Click btnEqDom)

mapButton :: (MonadFix m, DomBuilder t m) => a -> Text -> m (Event t a)
mapButton a txt = (a <$) <$> button txt