{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

module Parser.Parser ( parse ) where

-----------------------------------------------------------------------------

import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text ( Text )

import Control.Monad ( void )

import Text.Megaparsec.Char ( space1, char )

import Text.Megaparsec ( single, between, manyTill)

import Ast ( UnaryOp (..)
           , BinOp (..)
           , Type (..)
           , Expr (..)
           , Bind (..)
           , Statement (..)
           , Struct (..)
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
                    )

-----------------------------------------------------------------------------

parse :: Text -> ()
parse = undefined











