module AoC (
  Parser,
  module Text.Megaparsec, 
  module Text.Megaparsec.Char,
  module Text.Megaparsec.Char.Lexer,
  module Text.Pretty.Simple,
  module Control.Monad,
  module Control.Applicative,
  module Data.List,
  module Data.Function,
  module Data.Maybe,
  module Data.Char,
  T.Text,
) where

import Text.Megaparsec(Parsec, parse, manyTill, anySingle, errorBundlePretty, many, eof, choice)
import Text.Megaparsec.Char(eol, letterChar, digitChar, string, char)
import Text.Megaparsec.Char.Lexer(decimal)

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Char (digitToInt)

import Control.Monad (void)
import Control.Applicative ((<|>))

import Text.Pretty.Simple (pPrint)
import qualified Distribution.PackageDescription as Text.Pretty

type Parser = Parsec Void T.Text

parseAndApply :: Show b => Parser a -> (a -> b) -> FilePath -> IO ()
parseAndApply p f inputfile = do
  content <- readFile inputfile
  let parseResult = parse p "" (T.pack content)
  case parseResult of
    Left err -> putStrLn $ "Failed with error: " ++ errorBundlePretty err
    Right input -> do putStrLn "Answer: " 
                      pPrint $ f input

parseTestAndSolve :: (Show b, Eq b) => Parser a -> (a -> b) -> [(T.Text,b)] -> FilePath -> IO ()   
parseTestAndSolve parseFn solveFn tests inputFile = do
  let parsedTests = traverse parseTest tests
  case parsedTests of
    Left err -> putStrLn $ "Failed to parse tests: " ++ errorBundlePretty err
    Right inputs -> do 
      putStrLn "Running tests...\n"
      testResults <- traverse verifyTestResult (zip tests (map solveFn inputs))
      if and testResults
      then do 
        putStrLn "\nAll tests passed!"
        putStrLn "Running solution on real test input...\n"
        parseAndApply parseFn solveFn inputFile
      else putStrLn "\nTests failed. Skipping solution..."
      return ()
  where 
    parseTest (input, _) = parse parseFn "" input

    -- verifyTestResult :: (Show a, Eq a) => ((T.Text,a),a) -> IO Bool
    verifyTestResult ((input,expectedOutput), output) =
      if output == expectedOutput
      then do
        TIO.putStrLn $ "[Ok]\t\t'" <> input <> "'\n\t\t= " <> (T.pack . show) output
        return True
      else do
        TIO.putStrLn $ "[Failed]\t'" <> input <> "'\n\t\t= " <> (T.pack . show) output 
                    <> " (Expected " <> (T.pack . show) expectedOutput <> ")"
        return False
