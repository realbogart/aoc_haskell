module Y2019.D4 where

import AoC
import Data.List.GroupBy qualified as G

default (Text, Int)

parseInput :: Parser (Int, Int)
parseInput = do
  from <- decimal <* char '-'
  to <- decimal
  return (from, to)

partOneTests = []

partTwoTests = []

digitsIncrease :: Int -> Bool
digitsIncrease = (== 1) . length . G.groupBy (<=) . map digitToInt . show

twoAdjacentMatch :: Int -> Bool
twoAdjacentMatch = any ((> 1) . length) . group . map digitToInt . show

onlyTwoAdjacentMatch :: Int -> Bool
onlyTwoAdjacentMatch = any ((== 2) . length) . group . map digitToInt . show

partOne (from, to) = length $ filter twoAdjacentMatch $ filter digitsIncrease range
  where
    range = [from .. to]

partTwo (from, to) = length $ filter onlyTwoAdjacentMatch $ filter digitsIncrease range
  where
    range = [from .. to]
