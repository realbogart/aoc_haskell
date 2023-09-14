module Y2017.D2 where

import AoC

default (Text, Int)

parseInput = parseLineSeparated row
  where row = some (decimal <* optional hspace1)

partOneTests = [("5 1 9 5\n7 5 3\n2 4 6 8", 18)]

partOne :: [[Int]] -> Int
partOne = sum . map sum
