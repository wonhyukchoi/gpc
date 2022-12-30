module Ast where

-----------------------------------------------------------------------------

import Data.Text ( Text )

-----------------------------------------------------------------------------

data BinOp
  = Add
  | Sub
  | Mult
  | Div
  | Power
  | Assign
  | Eq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  deriving (Show, Eq)

data UnaryOp = Neg | Not deriving (Show, Eq)

data Expr
  = IntLiteral Integer
  | StrLiteral Text
  | FloatLiteral Double
  | CharLiteral Char
  | BoolLiteral Bool
  | Var Text
  | BinaryOp BinOp Expr Expr
  | UnaryOp UnaryOp Expr
  -- | FunctionCall Text [Expr]
  deriving (Eq, Show)

data Type
  = IntType
  | FloatType
  | BoolType
  | CharType
  | StrType
  | VoidType
  | StructType Text
  deriving (Show, Eq)

data Bind = Bind {bindType :: Type, bindName :: Text} deriving (Show, Eq)

data Statement
  = Expr Expr
  -- | Return Expr
  | Declare Bind 
  | Define Bind Expr
  -- | If Expr Statement
  -- | For Expr Expr Expr Statement
  -- | While Expr Statement
  deriving (Show, Eq)

-- data Struct = Struct {structName :: Text, fields :: [Bind]} deriving (Show, Eq)

data Function = Function { funcType :: Type
                         , funcName :: Text
                         , args     :: [Bind]
                         , body     :: [Statement]
                         }
                       deriving (Show, Eq)

-- data Program = Program [Struct] [Function] deriving (Eq, Show)
newtype Program = Program [Function] deriving (Eq, Show)