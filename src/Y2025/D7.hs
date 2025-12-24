module Y2025.D7 where

import AoC
import Data.HashMap.Strict qualified as HMS
import Data.IORef
import Data.List (nub)
import Data.Text qualified as T
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (MonadParsec (takeWhileP), someTill)

default (Text, Int)

type Input = (Int, [[Int]])

type SplitMap = HMS.HashMap (Int, Int) Int

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

input = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............\n"

partOneTests = [(input, 21)]

partTwoTests = [(input, 40)]

splitBeam :: [Int] -> Int -> [Int]
splitBeam splitters beam
  | beam `elem` splitters = [beam - 1, beam + 1]
  | otherwise = [beam]

splitBeams :: (Int, [Int]) -> [Int] -> (Int, [Int])
splitBeams (acc, beams) splitters = (acc + num_splits, (nub . concat) splits)
  where
    splits = map (splitBeam splitters) beams
    num_splits = length $ filter (== 2) $ map length splits

recursiveSplit :: IORef SplitMap -> Int -> Int -> [[Int]] -> IO Int
recursiveSplit _ _ _ [] = return 1
recursiveSplit ref_splitmap level beam (splitters : rest) = do
  splitmap <- readIORef ref_splitmap
  case HMS.lookup (beam, level) splitmap of
    Just stored -> return stored
    Nothing ->
      if beam `elem` splitters
        then do
          count_left <- recursiveSplit ref_splitmap next_level (beam - 1) rest
          count_right <- recursiveSplit ref_splitmap next_level (beam + 1) rest
          let current_count = count_left + count_right
          modifyIORef ref_splitmap (HMS.insert (beam, level) current_count)
          return current_count
        else recursiveSplit ref_splitmap next_level beam rest
  where
    next_level = level + 1

partOne :: Input -> Int
partOne (start, rows) = num_splits
  where
    (num_splits, _) = foldl' splitBeams (0, [start]) rows

partTwo :: Input -> Int
partTwo (start, rows) = unsafePerformIO $ do
  splitmap <- newIORef HMS.empty
  recursiveSplit splitmap 0 start rows
