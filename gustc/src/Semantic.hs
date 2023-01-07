-----------------------------------------------------------------------------

module Semantic ( analyze ) where

-----------------------------------------------------------------------------

import Ast
import SAst

-----------------------------------------------------------------------------
-- type Name = Text
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

-- | Semantic analysis.
-- Transforms an AST into a SAst.
analyze :: Program -> SAst
analyze = undefined