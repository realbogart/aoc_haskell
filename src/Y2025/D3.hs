module Y2025.D3 where

import AoC
import Data.List.Extra

default (Text, Int)

type Battery = Int

type Bank = [Battery]

parseInput :: Parser [Bank]
parseInput = parseLineSeparated (some (digitToInt <$> digitChar))

partOneTests = [("987654321111111\n811111111111119\n234234234234278\n818181911112111", 357)]

partTwoTests = [("987654321111111\n811111111111119\n234234234234278\n818181911112111", 3121910778619)]

calcJoltage :: Int -> Bank -> Int
calcJoltage end bank
  | end > 0 = max_left * (10 ^ end) + calcJoltage (end - 1) right
  | otherwise = maximum bank
  where
    max_left = maximum (dropEnd end bank)
    i = fromMaybe 0 $ elemIndex max_left bank
    (_, right) = splitAt (i + 1) bank

partOne :: [Bank] -> Int
partOne = sum . map (calcJoltage 1)

partTwo :: [Bank] -> Int
partTwo = sum . map (calcJoltage 11)
