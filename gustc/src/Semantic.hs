{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

module Semantic ( analyze ) where

-----------------------------------------------------------------------------

import qualified Data.Map as Map

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except ( ExceptT, throwE )
import Control.Monad.Trans.State ( gets, put, State )
import Data.Text (Text)

import Ast
import SAst

-----------------------------------------------------------------------------

-- -- Tells us if a binding appears in a function, a struct, or
-- -- is a global variable.
-- data BindingLoc = F Function | S Struct | Toplevel 
--   deriving Show
-- data SemantError =
--     IllegalBinding Name BindingKind VarKind BindingLoc
--   | UndefinedSymbol Name SymbolKind Expr
--   | TypeError { expected :: [Type], got :: Type, errorLoc :: Statement }
--   | CastError { to :: Type, from :: Type, castLoc :: Statement }
--   | ArgError { nExpected :: Int, nGot :: Int, callSite :: Expr }
--   | Redeclaration Name
--   | NoMain
--   | AddressError Expr
--   | AssignmentError { lhs :: Expr, rhs :: Expr }
--   | AccessError { struct :: Expr, field :: Expr }
--   | DeadCode Statement -- ^ For statements in a block following a return
--   deriving (Show)
-- data BindingKind = Duplicate | Void deriving (Show)
-- data SymbolKind = Var | Func deriving (Show)

-- data VarKind = Global | Formal | Local | StructField
--   deriving (Show, Eq, Ord)

-- type Vars = M.Map (Text, VarKind) Type
-- type Funcs = M.Map Text Function
-- type Structs = [Struct]
-- data Env = Env { vars     :: Vars
--                , funcs    :: Funcs
--                , structs  :: Structs
--                }

-- type Semant = ExceptT SemantError (State Env)

data Env = Env { variables :: Map.Map Text Type
               , functions :: Map.Map Text Function
               }

data SemanticError
  = UndefinedSymbol Text Expr
  | Redeclaration Text Statement
  | TypeError {expected :: Type, got :: Type}
  deriving (Show)

type Context = ExceptT SemanticError (State Env)

-- data BindingKind = Duplicate | Void deriving (Show)
-- data SymbolKind = Var | Func deriving (Show)

-- data VarKind = Global | Formal | Local | StructField
--   deriving (Show, Eq, Ord)

-- | Semantic analysis.
-- Transforms an AST into a SAst.
analyze :: Program -> Either SemanticError SProgram
analyze (Program functions) = SProgram <$> mapM checkFunction functions

checkExpr :: Expr -> Context SExpr
checkExpr = \case
  IntLiteral int -> return $ SIntLiteral int
  StrLiteral str -> return $ SStrLiteral str
  FloatLiteral f -> return $ SFloatLiteral f
  CharLiteral char -> return $ SCharLiteral char
  BoolLiteral bool -> return $ SBoolLiteral bool
  expr@(Var variable) -> do
    vars <- lift $ gets variables
    if defined variable vars
      then return $ LValue $ SVar variable
      else throwE $ UndefinedSymbol variable expr
  BinaryOp binOp lhs rhs -> SBinaryOp binOp <$> checkExpr lhs <*> checkExpr rhs
  UnaryOp unaryOp expr -> SUnaryOp unaryOp <$> checkExpr expr

defined :: Text -> Map.Map Text a -> Bool
defined name definedNames = Map.member name definedNames

checkStatement :: Statement -> Context SStatement
checkStatement = \case
  Expr expr -> SExpr <$> checkExpr expr
  stmt@(Declare (Bind{..})) ->
    if not $ defined bind
      then return $ SDecl bind
      else Left $ Redeclaration (bindName bind) stmt
  stmt@(Define (Bind{..}) expr) ->
    if not $ defined bind
      then SDef bind <$> checkExpr expr
      else Left $ Redeclaration (bindName bind) stmt

checkFunction :: Function -> Either SemanticError SFunction
checkFunction Function{..} = 
  SFunction funcType funcName args <$> mapM checkStatement body