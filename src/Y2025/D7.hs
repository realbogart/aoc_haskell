module Y2025.D7 where

import AoC
import Data.List (nub)
import Data.Text qualified as T
import Text.Megaparsec (MonadParsec (takeWhileP), someTill)

default (Text, Int)

type Input = (Int, [[Int]])

parseInput :: Parser Input
parseInput = do
  start <- T.length <$> takeWhileP (Just "spaces") (== '.') <* someTill anySingle eol
  all_splitters <- parseLineSeparated parseSplitters
  return (start, all_splitters)
  where
    parseSplitters :: Parser [Int]
    parseSplitters = scanl1 (\a b -> a + b + 1) . init <$> manyTill parseSplitter eol

    parseSplitter :: Parser Int
    parseSplitter = length <$> some (char '.') <* optional (char '^')

partOneTests =
  [ (".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............\n", 21)
  ]

splitBeam :: [Int] -> Int -> [Int]
splitBeam splitters beam
  | beam `elem` splitters = [beam - 1, beam + 1]
  | otherwise = [beam]

splitBeams :: (Int, [Int]) -> [Int] -> (Int, [Int])
splitBeams (acc, beams) splitters = (acc + num_splits, (nub . concat) splits)
  where
    splits = map (splitBeam splitters) beams
    num_splits = length $ filter (== 2) $ map length splits

partOne :: Input -> Int
partOne (start, rows) = num_splits
  where
    (num_splits, _) = foldl' splitBeams (0, [start]) rows
