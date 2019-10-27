{-# LANGUAGE OverloadedStrings #-}
module Eval.Parse where

import Eval.AST

import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr


type Parser = Parsec Void Text

parseExp :: String -> Either String AST
parseExp input =
  case parse expP "" (pack input) of
    Left ebundle -> Left (errorBundlePretty ebundle)
    Right ast    -> Right ast


sc :: Parser ()
sc = L.space space1 empty empty

lexeme  = L.lexeme sc

parensP :: Parser a -> Parser a
parensP = between (L.symbol sc "(") (L.symbol sc ")")

integerP :: Parser Integer
integerP = L.signed sc (lexeme L.decimal)

doubleP :: Parser Double
doubleP = L.signed sc (lexeme L.float)

expP :: Parser AST
expP = makeExprParser termP table <?> "expression"

termP :: Parser AST
termP = ( Val    <$> (   try doubleP
                     <|> (fromInteger <$> integerP)
                     )
        )
    <|> ( Parens <$> parensP expP )

table = [ [ binary  "×"  (BinOp OpMul), binary  "÷"  (BinOp OpDiv)  ]
        , [ binary  "+"  (BinOp OpAdd), binary  "-"  (BinOp OpSub)  ]
        , [ rbinary "^"  (BinOp OpPow)]
        ]

-- Left associative binary operation
binary  name f = InfixL  (f <$ L.symbol sc name)

-- Right associative binary operation
rbinary name f = InfixR  (f <$ L.symbol sc name)