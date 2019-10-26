{-# LANGUAGE OverloadedStrings #-}
module Style where

import Clay
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

calcStyle :: ByteString
calcStyle = encodeUtf8 $ toStrict $ render $ do
  ".calc-item" ? do
    borderRadius (px 10) (px 10) (px 10) (px 10)
    background (rgb 0x3D 0x3C 0x42)
    color white
    fontSize (px 23)
    justifyContent center
    width (pct 100)
