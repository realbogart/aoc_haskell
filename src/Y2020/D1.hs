module Y2020.D1 where

import AoC

default (Text, Int)

parseInput = parseLineSeparated decimal

input = "1721\n979\n366\n299\n675\n1456\n"

partOneTests = [(input, [514579])]
partTwoTests = [(input, [241861950])]

partOne :: [Int] -> [Int]
partOne = map product . filter ((==2020) . sum) . combinationsOfTwo
  where combinationsOfTwo xs = [ [x,y] | (x:ys) <- tails xs, y <- ys ]

partTwo :: [Int] -> [Int]
partTwo = map product . filter ((==2020) . sum) . combinationsOfThree
  where combinationsOfThree xs = [ [x,y,z] | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs ]

