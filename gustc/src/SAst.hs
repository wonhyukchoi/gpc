module SAst where

-----------------------------------------------------------------------------

import Data.Text ( Text )

import Ast

-----------------------------------------------------------------------------

data LValue = SVar Text deriving (Eq, Show)

type TypedExpr = (Type, SExpr)

-- | Semantically typed expressions.
data SExpr
  = SIntLiteral Integer
  | SStrLiteral Text
  | SFloatLiteral Double
  | SCharLiteral Char
  | SBoolLiteral Bool
  | SBinaryOp BinOp TypedExpr TypedExpr
  | SUnaryOp UnaryOp TypedExpr
  | LValue LValue
  | SAssign LValue TypedExpr
  -- | FunctionCall Text [Expr]
  deriving (Eq, Show)

data SStatement
  = SExpr TypedExpr
  | SReturn TypedExpr
  | SDecl Bind
  | SDef  Bind TypedExpr
  deriving (Eq, Show)

data SFunction = SFunction { sFuncType :: Type
                           , sFuncName :: Text
                           , sArgs     :: [Bind]
                           , sBody     :: [SStatement]
                           }
                          deriving (Show, Eq)

newtype SProgram = SProgram [SFunction] deriving (Eq, Show)
