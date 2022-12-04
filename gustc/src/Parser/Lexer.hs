{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

module Parser.Lexer where

-----------------------------------------------------------------------------

import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text ( Text )

import Control.Monad ( void )

import Text.Megaparsec.Char ( space1, char )

import Text.Megaparsec ( single, between, manyTill)

import Parser.Common ( Parser )

-----------------------------------------------------------------------------

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipLineComment "/* */"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

dquotes :: Parser a -> Parser a
dquotes = between (single '"') (single '"')

squotes :: Parser a -> Parser a
squotes = between (single '\'') (single '\'')

semicolon :: Parser ()
semicolon = void $ symbol ";"

comma :: Parser ()
comma = void $ symbol ","

asterik :: Parser ()
asterik = void $ symbol "*"

reservedWords :: [Text]
reservedWords = [ "if"
                , "else"
                , "else if"
                , "for"
                , "while"
                , "true"
                , "false"
                , "int"
                , "bool"
                , "char"
                , "float"
                , "void"
                , "return"
                , "struct"
                ]

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

strLiteral :: Parser String
strLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

intLiteral :: Parser Integer
intLiteral = L.signed (return ()) $ lexeme L.decimal

floatLiteral :: Parser Double
floatLiteral = L.signed (return ()) $ lexeme L.float
