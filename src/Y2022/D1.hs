module Y2022.D1 where

import AoC

default (Int, Text)

parseInput = parseGroupsLineSeparated decimal

input = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

partOneTests = [(input, 24000)]

partTwoTests = [(input, 45000)]

partOne :: [[Int]] -> Int
partOne = maximum . map sum

partTwo :: [[Int]] -> Int
partTwo = sum . take 3 . reverse . sort . map sum
