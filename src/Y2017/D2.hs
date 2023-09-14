module Y2017.D2 where

import AoC

default (Text, Int)

parseInput = parseLineSeparated row
  where row = some (decimal <* optional hspace1)

partOneTests = [("5 1 9 5\n7 5 3\n2 4 6 8", 18)]
partTwoTests = [("5 9 2 8\n9 4 7 3\n3 8 6 5", 9)]

partOne = sum . map minMaxDiff 
  where minMaxDiff :: [Int] -> Int
        minMaxDiff xs = abs $ subtract (minimum xs) (maximum xs)

partTwo = sum . map (sum . keepDivisable)
  where keepDivisable = map fst . filter (\(q,r) -> r == 0) . map (uncurry quotRem) . twoPerms
        twoPerms xs = [(x,y) | x <- xs, y <- xs, x/=y]

