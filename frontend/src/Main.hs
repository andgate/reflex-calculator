{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Eval

import Control.Monad
import Control.Monad.Fix (MonadFix)
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Text.Read (readMaybe)

import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign

import qualified Data.JSString as S

import System.IO.Unsafe

data CalcPad
  = PadNum Int
  | PadOp CalcOp
  | PadDot
  | PadBack
  | PadClear

data CalcOp
  = OpAdd
  | OpSub
  | OpDiv
  | OpMul
  | OpMod

main :: IO ()
main = mainWidget $
  el "table" $ mdo
    el "tr" $ do
      elAttr "td" ("colspan" =: "3")$ dynText inputText
      el "td" $ dynText resultText

    row1 <- el "tr" $ do
      btnClear <- el "td" $ mapButton PadClear "C"
      btnBack <- el "td" $ mapButton PadBack "<-"
      btnMod <- el "td" $ mapButton (PadOp OpMod) "%"
      btnAdd <- el "td" $ mapButton (PadOp OpAdd) "+"
      return [btnClear, btnBack, btnMod, btnAdd]
    
    row2 <- el "tr" $ do
      btn7 <- el "td" $ mapButton (PadNum 7) "7"
      btn8 <- el "td" $ mapButton (PadNum 8) "8"
      btn9 <- el "td" $ mapButton (PadNum 9) "9"
      btnSub <- el "td" $ mapButton (PadOp OpDiv) "-"
      return [btn7, btn8, btn9, btnSub]

    row3 <- el "tr" $ do
      btn4 <- el "td" $ mapButton (PadNum 4) "4"
      btn5 <- el "td" $ mapButton (PadNum 5) "5"
      btn6 <- el "td" $ mapButton (PadNum 6) "6"
      btnMul <- el "td" $ mapButton (PadOp OpMul) "*"
      return [btn4, btn5, btn6, btnMul]
    
    row4 <- el "tr" $ do
      btn1 <- el "td" $ mapButton (PadNum 1) "1"
      btn2 <- el "td" $ mapButton (PadNum 2) "2"
      btn3 <- el "td" $ mapButton (PadNum 3) "3"
      btnDiv <- el "td" $ mapButton (PadOp OpDiv) "/"
      return [btn1, btn2, btn3, btnDiv]
    
    (row5, btnEq) <- el "tr" $ do
      btnDot <- el "td" $ mapButton PadDot "."
      btn0 <- el "td" $ mapButton (PadNum 0) "0"
      btnEq <- elAttr "td" ("colspan" =: "2") (buttonAttr () ("style" =: "width: 100%") "=")
      return ([btnDot, btn0], btnEq)
    
    let padEv = (leftmost . mconcat) [row1, row2, row3, row4, row5]
    inputText <- foldDyn updateInputBox mempty padEv

    let result = (pack . show . eval . unpack) <$> inputText
    -- let numberString = maybe "???" (pack . show) <$> result
    resultText <- holdDyn "0" $ tag (current result) btnEq

    return ()

updateInputBox :: CalcPad -> Text -> Text
updateInputBox padOp input = case padOp of
  PadNum i -> input <> (pack . show) i
  PadOp arithOp -> case arithOp of
    OpAdd -> input <> "+"
    OpSub -> input <> "-"
    OpMul -> input <> "*"
    OpDiv -> input <> "/"
    OpMod -> input <> "%"
  PadDot -> input <> "."
  PadBack -> if Text.null input then "" else Text.init input
  PadClear -> mempty

buttonAttr :: (DomBuilder t m) => a -> Map Text Text -> Text -> m (Event t a)
buttonAttr a attrs label = do
  (btnEqDom, _) <- elAttr' "button" attrs $ text label
  return (a <$ domEvent Click btnEqDom)

mapButton :: (MonadFix m, DomBuilder t m) => a -> Text -> m (Event t a)
mapButton a txt = (a <$) <$> button txt