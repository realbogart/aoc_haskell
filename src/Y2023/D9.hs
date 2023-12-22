module Y2023.D9 where

import AoC

default (Int, Text)

partOneTests = [("0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45", 114)]

parseInput = parseLineSeparated (some parseSignedInteger)

differences :: [Int] -> [Int]
differences l = map (uncurry (flip (-))) pairs
  where pairs :: [(Int, Int)]
        pairs = zip l (tail l)

pyramidNext :: [Int] -> Int
pyramidNext l | all (== 0) l = 0
              | otherwise = last l + pyramidNext (differences l)

partOne :: [[Int]] -> Int
partOne = sum . map pyramidNext
