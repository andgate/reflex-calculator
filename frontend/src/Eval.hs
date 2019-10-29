module Eval where

import Eval.Parse
import Eval.AST

import Data.Either

import Data.Text (Text)

eval :: Text -> Maybe Double
eval ins = either (const Nothing) Just (simplify <$> parseExp ins)