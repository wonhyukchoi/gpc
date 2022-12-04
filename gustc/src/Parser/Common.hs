module Parser.Common (Parser) where

-----------------------------------------------------------------------------

import Data.Void (Void)

import Data.Text (Text)

import Text.Megaparsec (Parsec)

-----------------------------------------------------------------------------

type Parser = Parsec Void Text
