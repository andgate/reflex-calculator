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
  | PadOp CalcOp
  | PadDot
  | PadBack
  | PadClear
  | PadEq
  | PadInit

data CalcOp
  = OpAdd
  | OpSub
  | OpDiv
  | OpMul
  | OpMod



main :: IO ()
main = mainWidgetWithCss calcStyle $
  elClass "table" "calc-body" $ mdo
    el "tr" $ do
      elAttr "td" ("colspan" =: "4" <> "class" =: "calc-screen") $
        dynText screenText

    row1 <- el "tr" $ do
      btnClear <- el "td" $ buttonClass PadClear "calc-item clear" "C"
      btnBack <- el "td" $ buttonClass PadBack "calc-item back" "←"
      btnMod <- el "td" $ calcButton (PadOp OpMod) "%"
      btnAdd <- el "td" $ calcButton (PadOp OpAdd) "+"
      return [btnClear, btnBack, btnMod, btnAdd]
    
    row2 <- el "tr" $ do
      btn7 <- el "td" $ numButton (PadNum 7) "7"
      btn8 <- el "td" $ numButton (PadNum 8) "8"
      btn9 <- el "td" $ numButton (PadNum 9) "9"
      btnSub <- el "td" $ calcButton (PadOp OpSub) "-"
      return [btn7, btn8, btn9, btnSub]

    row3 <- el "tr" $ do
      btn4 <- el "td" $ numButton (PadNum 4) "4"
      btn5 <- el "td" $ numButton (PadNum 5) "5"
      btn6 <- el "td" $ numButton (PadNum 6) "6"
      btnMul <- el "td" $ calcButton (PadOp OpMul) "×"
      return [btn4, btn5, btn6, btnMul]
    
    row4 <- el "tr" $ do
      btn1 <- el "td" $ numButton (PadNum 1) "1"
      btn2 <- el "td" $ numButton (PadNum 2) "2"
      btn3 <- el "td" $ numButton (PadNum 3) "3"
      btnDiv <- el "td" $ calcButton (PadOp OpDiv) "÷"
      return [btn1, btn2, btn3, btnDiv]
    
    row5 <- el "tr" $ do
      btnDot <- el "td" $ calcButton PadDot "."
      btn0 <- el "td" $ numButton (PadNum 0) "0"
      btnEq <- elAttr "td" ("colspan" =: "2") (calcButton PadEq "=")
      return [btnDot, btn0, btnEq]
    
    let padEv = (leftmost . mconcat) [row1, row2, row3, row4, row5]
    screenText <- foldDyn updateInputBox "0" padEv

    return ()


updateInputBox :: CalcPad -> Text -> Text
updateInputBox padOp "0" = updateInputBox padOp mempty 
updateInputBox padOp input
  | Text.length input == 10 && opAppends padOp = input
  | otherwise = case padOp of
      PadNum i ->
        input <> (pack . show) i

      PadOp arithOp ->
        case arithOp of
          OpAdd -> input <> "+"
          OpSub -> input <> "-"
          OpMul -> input <> "×"
          OpDiv -> input <> "÷"
          OpMod -> input <> "%"
      PadDot -> input <> "."

      PadBack | Text.length input <= 1 -> "0"
              | otherwise              -> Text.init input
      
      PadClear -> "0"
      PadEq -> pack . show . eval . unpack $ input

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