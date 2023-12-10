module AoC (
  Parser,
  module Text.Megaparsec, 
  module Text.Megaparsec.Char,
  module Text.Megaparsec.Char.Lexer,
  module Text.Pretty.Simple,
  module Control.Monad,
  module Control.Applicative,
  module Data.Function,
  module Data.Maybe,
  module Data.Char,
  module Data.List.Split,
  module Debug.Trace,
  T.Text,
  parseLineSeparated,
  parseGroupsLineSeparated,
  comb2,
  perm2,
  T.pack,
  parseAndApply,
  parseTestAndSolve,
  L.sort,
  L.sortBy,
  L.foldl',
  L.inits,
  L.scanl,
  L.scanl',
  L.group,
  L.groupBy,
  L.tails,
  L.find,
  L.delete,
  L.isPrefixOf,
  L.singleton,
  minimumBy,
  maximumBy
) where

import Debug.Trace

import Text.Megaparsec(Parsec, parse, manyTill, anySingle, errorBundlePretty, many, eof, choice, optional, some, count, anySingleBut, getOffset, notFollowedBy)
import Text.Megaparsec.Char(eol, letterChar, numberChar, digitChar, string, char, tab, space, spaceChar, hspace, hspace1, latin1Char)
import Text.Megaparsec.Char.Lexer(decimal)

import Data.List qualified as L
import Data.Text qualified as T
import Data.List.Split
import Data.Text.IO qualified as TIO
import Data.Void
import Data.Function (on)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import Data.Char (digitToInt, ord, chr, isUpper, isDigit)
import Data.Foldable (minimumBy, maximumBy)

import Control.Monad (void)
import Control.Applicative ((<|>))

import Text.Pretty.Simple (pPrint)

comb2 xs = [(x,y) | (x:ys) <- L.tails xs, y <- ys]
perm2 xs = [(x,y) | x <- xs, y <- xs, x /= y]

type Parser = Parsec Void T.Text

parseLineSeparated :: Parser a -> Parser [a]
parseLineSeparated p = some (p <* optional eol) 

parseGroupsLineSeparated :: Parser a -> Parser [[a]]
parseGroupsLineSeparated p = some (parseLineSeparated p <* optional eol) 

parseAndApply :: Show b => Parser a -> (a -> b) -> FilePath -> IO ()
parseAndApply p f inputfile = do
  content <- readFile inputfile
  let parseResult = parse p "" (T.pack content)
  case parseResult of
    Left err -> putStrLn $ "Failed with error: " ++ errorBundlePretty err
    Right input -> do putStrLn "Answer: " 
                      pPrint $ f input

parseTestAndSolve :: (Show a, Show b, Eq b) => Parser a -> (a -> b) -> [(T.Text,b)] -> FilePath -> IO ()   
parseTestAndSolve parseFn solveFn tests inputFile = do
  let parsedTests = traverse parseTest tests
  case parsedTests of
    Left err -> putStrLn $ "Failed to parse tests: " ++ errorBundlePretty err
    Right inputs -> do 
      putStrLn "Running tests...\n"
      testResults <- traverse verifyTestResult (zip3 tests inputs (map solveFn inputs))
      if and testResults
      then do 
        putStrLn "\nAll tests passed!"
        putStrLn "Running solution on real test input...\n"
        parseAndApply parseFn solveFn inputFile
      else putStrLn "\nTests failed. Skipping solution..."
      return ()
  where 
    parseTest (input, _) = parse parseFn "" input

    verifyTestResult ((_,expectedOutput), input, output) =
      if output == expectedOutput
      then do
        TIO.putStrLn $ "[Ok]\t\t" <> (T.pack . show) input <> "\n\t\t= " <> (T.pack . show) output
        return True
      else do
        TIO.putStrLn $ "[Failed]\t" <> (T.pack . show) input <> "\n\t\t= " <> (T.pack . show) output 
                    <> " (Expected " <> (T.pack . show) expectedOutput <> ")"
        return False
