module SAst where

-----------------------------------------------------------------------------

import Data.Text ( Text )

import Ast

-----------------------------------------------------------------------------

data LValue = SVar Text deriving (Eq, Show)

-- | Semantically typed expressions.
data SExpr
  = SIntLiteral Integer
  | SStrLiteral Text
  | SFloatLiteral Double
  | SCharLiteral Char
  | SBoolLiteral Bool
  | SBinaryOp BinOp SExpr SExpr
  | SUnaryOp UnaryOp SExpr
  | LValue LValue
  | SAssign LValue SExpr
  -- | FunctionCall Text [Expr]
  deriving (Eq, Show)

data SStatement
  = SExpr SExpr
  | SDecl Bind
  | SDef  Bind SExpr
  deriving (Eq, Show)

data SFunction = SFunction { sFuncType :: Type
                           , sFuncName :: Text
                           , sArgs     :: [Bind]
                           , sBody     :: [SStatement]
                           }
                          deriving (Show, Eq)

newtype SProgram = SProgram [SFunction] deriving (Eq, Show)