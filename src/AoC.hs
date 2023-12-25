module AoC (
  Parser,
  module Text.Megaparsec, 
  module Text.Megaparsec.Char,
  module Text.Pretty.Simple,
  module Control.Monad,
  module Control.Applicative,
  module Data.Function,
  module Data.Maybe,
  module Data.Char,
  module Debug.Trace,
  module Data.Ord,
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
  Lex.decimal,
  Lex.signed,
  Split.chunksOf,
  Split.splitOn,
  lexeme,
  minimumBy,
  maximumBy,
  sc,
  parseInteger,
  parseSignedInteger,
  Grid (..),
  GridCoord,
  getGrid,
  getGridValue,
) where

import Debug.Trace

import Text.Megaparsec(Parsec, parse, manyTill, anySingle, errorBundlePretty, many, eof, choice, optional, some, count, anySingleBut, getOffset, notFollowedBy, empty)
import Text.Megaparsec.Char(eol, letterChar, numberChar, digitChar, string, char, tab, space, space1, spaceChar, hspace, hspace1, latin1Char)
import Text.Megaparsec.Char.Lexer qualified as Lex

import Data.List qualified as L
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.List.Split qualified as Split
import Data.Text.IO qualified as TIO
import Data.Void
import Data.Function (on)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import Data.Char (digitToInt, ord, chr, isUpper, isDigit)
import Data.Foldable (minimumBy, maximumBy)
import Data.Ord (comparing, Down(..))

import Control.Monad (void)
import Control.Applicative ((<|>))

import Text.Pretty.Simple (pPrint)

comb2 xs = [(x,y) | (x:ys) <- L.tails xs, y <- ys]
perm2 xs = [(x,y) | x <- xs, y <- xs, x /= y]

type Parser = Parsec Void T.Text

default (T.Text, Int)

type GridCoord = (Int, Int)

data Grid a = Grid
  { grid :: V.Vector a
  , width :: Int
  , height :: Int
  }

-- data GridNeighbours = GridNeighbours
--   { topLeft :: GridCoord
--   , top :: GridCoord
--   , topRight :: GridCoord
--   , right :: GridCoord
--   , bottomRight :: GridCoord
--   , bottom :: GridCoord
--   , bottomLeft :: GridCoord
--   }

instance Show a => Show (Grid a) where
    show (Grid grid width height) = "\n" ++ concatMap (showRow . getRow) rowIndices
      where getRow startIndex = V.toList $ V.slice startIndex width grid  
            showRow r = L.intersperse ' ' (concatMap showNoQuotes r) ++ "\n"
            rowIndices = [0, width..width*(height-1)]
            showNoQuotes = tail . init . show

getGrid :: Eq a => a -> [a] -> Grid a
getGrid delim cs = Grid (V.fromList flat) width height
  where rows = Split.splitOn [delim] cs
        height = length rows
        width | height == 0 = 0
              | otherwise = length (head rows)
        flat = filter (/= delim) cs

getGridValue :: Grid a -> GridCoord -> a
getGridValue grid (x, y) = grid.grid V.! (y * grid.width + x)

isInsideGrid :: Grid a -> GridCoord -> Bool
isInsideGrid grid (x, y) = x >= 0 && y >= 0 && x < grid.width && y < grid.height

getGridNeighbours :: Grid a -> GridCoord -> [GridCoord]
getGridNeighbours grid (x, y) = filter (isInsideGrid grid) ns
  where ns = [(x + 1, y + 1), (x + 1, y), (x + 1, y - 1), (x, y - 1), 
              (x, y + 1), (x - 1, y + 1), (x - 1, y), (x - 1, y + 1)]

parseLineSeparated :: Parser a -> Parser [a]
parseLineSeparated p = some (p <* optional eol) 

parseGroupsLineSeparated :: Parser a -> Parser [[a]]
parseGroupsLineSeparated p = some (parseLineSeparated p <* optional eol) 

sc = Lex.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

parseInteger = lexeme Lex.decimal
parseSignedInteger = Lex.signed sc parseInteger

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
