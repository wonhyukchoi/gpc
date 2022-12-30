{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

module Parser.Parser ( pProgram ) where

-----------------------------------------------------------------------------

import Data.Text ( pack )

import Text.Megaparsec ( many, some, try, (<|>) , sepBy, (<?>) )
import Control.Monad.Combinators.Expr ( Operator(..), makeExprParser )

import Ast
import Parser.Common ( Parser )
import Parser.Lexer

-----------------------------------------------------------------------------

pBoolLiteral :: Parser Expr
pBoolLiteral = (reserved "true"  >> return (BoolLiteral True))
          <|> (reserved "false" >> return (BoolLiteral False))

pLiteral :: Parser Expr
pLiteral = try (FloatLiteral <$> floatLiteral)
       <|> IntLiteral <$> intLiteral
       <|> CharLiteral <$> charLiteral
       <|> StrLiteral . pack <$> strLiteral
       <|> try pBoolLiteral

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" (UnaryOp Neg)
    , prefix "!" (UnaryOp Not)
    ]
  , [ binary "^" (BinaryOp Power)
    ]
  , [ binary "*" (BinaryOp Mult)
    , binary "/" (BinaryOp Div)
    ]
  , [ binary "+" (BinaryOp Add)
    , binary "-" (BinaryOp Sub)
    ]
  , [ binary "==" (BinaryOp Eq)
    , binary "<"  (BinaryOp Lt)
    , binary ">"  (BinaryOp Gt)
    , binary "<=" (BinaryOp Lte)
    , binary ">=" (BinaryOp Gte)
    ]
  , [ binary "&&" (BinaryOp And)
    , binary "||" (BinaryOp Or)
    ]
  , [ binary "=" (BinaryOp Assign)
    ]
  ]
  where binary  name f = InfixL  (f <$ symbol name)
        prefix  name f = Prefix  (f <$ symbol name)

pExpr :: Parser Expr
pExpr = makeExprParser exprTerm operatorTable <?> "expression"
  where exprTerm :: Parser Expr
        exprTerm = parens pExpr
               <|> try pLiteral
               <|> Var <$> identifier

pType :: Parser Type
pType = (reserved "int" >> return IntType)
    <|> (reserved "float" >> return FloatType)
    <|> (reserved "bool" >> return BoolType)
    <|> (reserved "char" >> return CharType)
    <|> (reserved "str" >> return StrType)
    <|> (reserved "void" >> return VoidType)
    -- TODO: custom struct types

pBind :: Parser Bind
pBind = Bind <$> pType <*> identifier

pStatement :: Parser Statement
pStatement = pStatement' <* semicolon
  where
   pStatement' = try pDefine'
             <|> (Declare <$> pBind)
             <|> (Expr <$> pExpr)

   pDefine' = do
     bind  <- pBind
     reserved "="
     Define bind <$> pExpr

pFunction :: Parser Function
pFunction = Function
  <$> pType
  <*> rawIdentifier
  <*> parens (pBind `sepBy` comma)
  <*> braces (many pStatement)

pProgram :: Parser Program
pProgram = Program <$> some pFunction