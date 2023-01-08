{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

module Semantic ( analyze ) where

-----------------------------------------------------------------------------

import qualified Data.Map as Map

import Control.Monad ( unless, when, void )
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except ( ExceptT, throwE )
import Control.Monad.Trans.State ( get, gets, modify, State )
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
  
addVar :: (Text, Type) -> Env -> Env
addVar (varName, varType) Env{..} = Env newVars functions
  where newVars = Map.insert varName varType variables

data SemanticError
  = UndefinedSymbol Text Expr
  | Redeclaration Text Statement
  | TypeError {expected :: [Type], got :: Type, errExpr :: Expr}
  | AssignmentError { assignLhs :: Expr, assignRhs :: Expr }
  deriving (Show)

type Context = ExceptT SemanticError (State Env)

-- data BindingKind = Duplicate | Void deriving (Show)
-- data SymbolKind = Var | Func deriving (Show)

-- data VarKind = Global | Formal | Local | StructField
--   deriving (Show, Eq, Ord)

-- | Semantic analysis.
-- Transforms an AST into a SAst.
analyze :: Program -> Context SProgram
analyze (Program functions) = SProgram <$> mapM checkFunction functions

isDefined :: Ord k => k -> Map.Map k v -> Bool
isDefined = Map.member

access :: Ord k => k -> Map.Map k v -> Maybe v
access = Map.lookup

assertSame :: Expr -> Type -> Type -> Context ()
assertSame expr type1 type2 = when
  (type1 /= type2)
  (throwE (TypeError [type1] type2 expr))

checkExpr :: Expr -> Context TypedExpr
checkExpr expr = case expr of
  IntLiteral int -> return (IntType, SIntLiteral int)
  StrLiteral str -> return (StrType, SStrLiteral str)
  FloatLiteral f -> return (FloatType, SFloatLiteral f)
  CharLiteral char -> return (CharType, SCharLiteral char)
  BoolLiteral bool -> return (BoolType, SBoolLiteral bool)

  Var varName -> do
    vars <- lift $ gets variables
    case access varName vars of
      Nothing -> throwE $ UndefinedSymbol varName expr
      Just varType -> return (varType, LValue $ SVar varName)

  BinaryOp binOp lhs rhs -> do
    lhs'@(lhsType, _) <- checkExpr lhs
    rhs'@(rhsType, _) <- checkExpr rhs
    void $ assertSame' lhsType rhsType

    let sexpr       = SBinaryOp binOp lhs' rhs'
        arithExpr   = assertNumeric lhsType >> return (lhsType, sexpr)
        compareExpr = assertNumeric lhsType >> return (BoolType, sexpr)
        booleanExpr = assertBoolean lhsType >> return (BoolType, sexpr)

    case binOp of
      Add   -> arithExpr
      Sub   -> arithExpr
      Mult  -> arithExpr
      Div   -> arithExpr 
      Power -> arithExpr 
      Lt    -> compareExpr 
      Gt    -> compareExpr 
      Lte   -> compareExpr 
      Gte   -> compareExpr 
      And   -> booleanExpr 
      Or    -> booleanExpr 

      -- In the future, this will be replaced with the Eq typeclass.
      Eq -> return (lhsType, sexpr)

      Assign -> case snd lhs' of
        LValue _ -> return (lhsType, sexpr)
        _ -> throwE $ AssignmentError lhs rhs

  UnaryOp unaryOp expr' -> do
    typedExpr@(exprType, _) <- checkExpr expr'
    case unaryOp of
      Neg -> assertNumeric exprType >> return (exprType, SUnaryOp Neg typedExpr)
      Not -> assertBoolean exprType >> return (exprType, SUnaryOp Not typedExpr)

  where
    isNumeric :: Type -> Bool
    isNumeric  = \case
      IntType   -> True
      FloatType -> True
      _         -> False
    
    assertSame' :: Type -> Type -> Context ()
    assertSame' = assertSame expr

    assertNumeric :: Type -> Context ()
    assertNumeric varType = unless
      (isNumeric varType)
      (throwE (TypeError [IntType, FloatType] varType expr))

    assertBoolean :: Type -> Context ()
    assertBoolean varType = unless
      (varType == BoolType)
      (throwE (TypeError [BoolType] varType expr))


checkStatement :: Function -> Statement -> Context SStatement
checkStatement Function{..} statement = do 
  env <- lift get
  let vars = variables env

  case statement of
    Expr expr -> SExpr <$> checkExpr expr

    Return expr -> do
      typedExpr@(exprType, _) <- checkExpr expr
      void $ assertSame expr funcType exprType
      return $ SReturn typedExpr 

    Declare bind@Bind{..} -> 
      if isDefined bindName vars
        then throwE $ Redeclaration bindName statement
        else do
          lift $ modify $ addVar (bindName, bindType)
          return $ SDecl bind

    Define bind@Bind{..} expr ->
      if isDefined bindName vars
        then throwE $ Redeclaration bindName statement
        else do
          lift $ modify $ addVar (bindName, bindType)
          SDef bind <$> checkExpr expr

checkFunction :: Function -> Context SFunction
checkFunction function@Function{..} = 
  SFunction funcType funcName args <$> mapM (checkStatement function) body