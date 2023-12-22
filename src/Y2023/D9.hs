module Y2023.D9 where

import AoC

default (Int, Text)

partOneTests = [("0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45", 114)]

parseInput = parseLineSeparated (some $ signed sc (lexeme decimal))

partOne :: [[Int]] -> Int
partOne _ = 54
