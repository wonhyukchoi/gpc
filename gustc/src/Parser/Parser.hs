{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module Parser.Parser ( pProgram ) where

-----------------------------------------------------------------------------

import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text ( Text )
import Control.Monad ( void )

import Text.Megaparsec.Char ( space1, char )
import Text.Megaparsec ( single, between, manyTill, runParser )

import Ast ( UnaryOp (..)
           , BinOp (..)
           , Type (..)
           , Expr (..)
           , Bind (..)
           , Statement (..)
        --    , Struct (..)
           , Function (..)
           , Program (..)
           )

import Parser.Common ( Parser )
import Parser.Lexer ( parens
                    , braces
                    , squotes
                    , dquotes
                    , semicolon
                    , comma
                    , asterik
                    , intLiteral
                    )

-----------------------------------------------------------------------------

pIntLiteral :: Parser Expr
pIntLiteral = IntLiteral <$> intLiteral

pLiteral :: Parser Expr
pLiteral = pIntLiteral

pExpr :: Parser Expr
pExpr = pLiteral

pProgram :: Parser Program
pProgram = undefined