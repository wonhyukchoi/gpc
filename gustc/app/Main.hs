{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

module Main (main) where

-----------------------------------------------------------------------------

import qualified Data.Text.IO as Text

import System.Exit ( die )

import Control.Monad.Trans.Except ( runExceptT )
import Control.Monad.Trans.State ( evalState )
import Control.Monad.IO.Class ( liftIO )

import Text.Megaparsec ( parse )

import qualified Interpreter as Interpreter
import Ast ( Program )
import SAst ( SProgram )
import Parser.Parser ( pProgram )
import Semantic ( analyze )
import Config ( Arguments(..), getArguments )

-----------------------------------------------------------------------------

main :: IO ()
main = do
  Arguments{..} <- getArguments
  if not interpret
    then putStrLn "No flag supplied. Exiting"
    else do
      ast  <- parser input
      sast <- semantic ast
      case Interpreter.interpret sast of
        Left err   -> die $ show err
        Right code -> putStrLn $ "Exited with exit code " ++ show code

parser :: FilePath -> IO Program
parser filepath = do
  contents <- Text.readFile filepath
  case parse pProgram filepath contents of
    Left    e -> die $ show e
    Right ast -> return ast

semantic :: Program -> IO SProgram
semantic program = case analyze program of
    Left e     -> die $ show e
    Right sast -> return sast