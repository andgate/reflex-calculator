module Eval where

import Eval.Parse
import Eval.AST

import Data.Either

eval :: String -> Double
eval ins = fromRight undefined (simplify <$> parseExp ins)