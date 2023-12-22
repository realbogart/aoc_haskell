module Y2023.D9 where

import AoC

default (Int, Text)

input = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

partOneTests = [(input, 114)]
partTwoTests = [(input, 2)]

parseInput = parseLineSeparated (some parseSignedInteger)

differences :: [Int] -> [Int]
differences l = map (uncurry (flip (-))) pairs
  where pairs :: [(Int, Int)]
        pairs = zip l (tail l)

pyramidNext :: [Int] -> Int
pyramidNext l | all (== 0) l = 0
              | otherwise = last l + pyramidNext (differences l)

pyramidPrevious :: [Int] -> Int
pyramidPrevious l | all (== 0) l = 0
                  | otherwise = head l - pyramidPrevious (differences l)

partOne :: [[Int]] -> Int
partOne = sum . map pyramidNext

partTwo :: [[Int]] -> Int
partTwo = sum . map pyramidPrevious
