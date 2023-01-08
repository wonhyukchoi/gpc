{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module Interpreter ( interpret ) where

-----------------------------------------------------------------------------

import qualified Data.Map as Map

import Data.Text (Text)
import Control.Monad ( unless, when, void )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Except ( ExceptT, throwE, runExceptT )
import Control.Monad.Trans.State ( get, gets, modify, put, State, evalState )

import Ast
import SAst

-----------------------------------------------------------------------------

data SymbolTable = SymbolTable { variables :: Map.Map Text Value
                               , functions :: Map.Map Text SFunction
                               }
                               deriving (Eq, Show)

getFunction :: Text -> SymbolTable -> Maybe SFunction
getFunction functionName SymbolTable{..} = Map.lookup functionName functions

functionNames :: SymbolTable -> [Text]
functionNames SymbolTable{..} = map fst $ Map.toList functions

updateVariable :: Text -> Value -> SymbolTable -> SymbolTable
updateVariable key value SymbolTable{..} = SymbolTable variables' functions
  where variables' = Map.insert key value variables

type ExitCode = Integer

data RuntimeError
  = NoMain [Text]
  | NoReturn Text
  | NonIntegerExitCode Type
  | Uninitialized Text SStatement
  deriving (Eq, Show)

type Runtime = ExceptT RuntimeError (State SymbolTable)

data Value
  = IntVal Integer
  | StrVal Text
  | FloatVal Float
  | CharVal Char
  | BoolVal Bool
  | VoidVal 
  deriving (Eq, Show)

-- | Interprets a program.
interpret :: SProgram -> Either RuntimeError ExitCode
interpret program = evalState (runExceptT  computation) emptyWorld
  where
    emptyWorld :: SymbolTable
    emptyWorld = SymbolTable Map.empty Map.empty

    computation :: Runtime ExitCode
    computation = do
      void $ initialize program
      symbolTable <- lift get
      case getFunction "main" symbolTable of
        Nothing  -> throwE $ NoMain $ functionNames symbolTable
        Just fxn -> do
          let mainReturnType = sFuncType fxn
          unless
            (mainReturnType == IntType)
            (throwE $ NonIntegerExitCode mainReturnType)
          returnValue <- apply fxn
          case returnValue of
            IntVal exitCode -> return exitCode
            _ -> error "This should never happen!"

initialize :: SProgram -> Runtime ()
initialize (SProgram fxns) = lift $ put $
    SymbolTable Map.empty (Map.fromList $ map (\f -> (sFuncName f, f)) fxns)

apply :: SFunction -> Runtime Value
apply SFunction{..} = apply' sBody
  where apply' :: [SStatement] -> Runtime Value
        apply' []         = throwE $ NoReturn sFuncName
        apply' (fxn:fxns) = case fxn of
          SReturn (_, expr) -> evaluate expr
          _                 -> execute fxn >> apply' fxns

execute :: SStatement -> Runtime Value
execute = \case
  SExpr _ -> return VoidVal
  SReturn _ -> error "This should never happen!"
  SDecl _ -> return VoidVal
  SDef Bind{..} (_, expr) -> do
    value <- evaluate expr
    lift $ modify (updateVariable bindName value)
    return VoidVal

evaluate :: SExpr -> Runtime Value
evaluate = undefined ()