{-# LANGUAGE OverloadedStrings #-}

module ParserTest (parseTests) where

import qualified Data.Text.IO as TextIO

import Data.Text ( Text )
import System.FilePath ( (</>) )
import System.Directory ( listDirectory )

import Text.Megaparsec ( parse )
import Test.Hspec ( describe, it, hspec )
import Test.Hspec.Megaparsec ( shouldSucceedOn )

import Parser.Parser ( pProgram )

import TestUtils ( buildExpectations )

parseTestFilesDir :: FilePath
parseTestFilesDir = "test/files"

parseTests :: IO ()
parseTests = do
  contents <- getFileContents
  hspec $
    describe "Parser Tests" $ buildExpectations parseSucceeds contents
  
  where
    getFileContents :: IO [Text]
    getFileContents = do
      fileNames <- listDirectory parseTestFilesDir
      let absolutePaths = map (parseTestFilesDir </>) fileNames
      mapM TextIO.readFile absolutePaths
    
    parseSucceeds input =
      it ">> " $ parse pProgram "" `shouldSucceedOn` input