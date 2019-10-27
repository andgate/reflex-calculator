{-# LANGUAGE OverloadedStrings #-}
module Style where

import Clay
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

calcStyle :: ByteString
calcStyle = encodeUtf8 $ toStrict $ render $ do
  ".calc-body" ? do
    borderRadius (px 10) (px 10) (px 10) (px 10)
    background (rgb 0xD7 0xD6 0xDA)
    border solid (px 10) black
    width (px 212)
    height (px 300)

  ".calc-screen" ? do
    borderRadius (px 10) (px 10) (px 10) (px 10)
    backgroundColor (rgb 0x9C 0x9F 0x96)
    color black
    textAlign (alignSide sideRight)
    width (pct 100)
    fontSize (px 32)
    paddingLeft (px 10)
    paddingRight (px 10)

  ".calc-item" ? do
    borderRadius (px 10) (px 10) (px 10) (px 10)
    background (rgb 0x3D 0x3C 0x42)
    color white
    fontSize (px 23)
    justifyContent center
    width (pct 100)
    height (pct 100)

  ".calc-item.num" ? do
      borderRadius (px 10) (px 10) (px 10) (px 10)
      background grey

  ".calc-item.clear" ? do
      background red

  ".calc-item.back" ? do
      background orangered
