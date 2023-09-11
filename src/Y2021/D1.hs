module Y2021.D1 where

import AoC
import Y2015.D1 (partOneTests)

parseInput :: Parser [Int]
parseInput = many (decimal <* (eol <|> "")) 

partOneTests :: [(Text, Int)]
partOneTests = [("199\n200\n208\n210\n200\n207\n240\n269\n260\n263",7)]

partTwoTests :: [(Text, Int)]
partTwoTests = [("199\n200\n208\n210\n200\n207\n240\n269\n260\n263",5)]

partOne :: [Int] -> Int
partOne xs = length $ filter (uncurry (<)) $ zip xs (drop 1 xs)

partTwo :: [Int] -> Int
partTwo xs = length $ filter (uncurry (<)) $ zip window (drop 1 window) 
  where window = zipWith3 (\a b c -> a + b + c) xs (drop 1 xs) (drop 2 xs)

