module Y2020.D1 where

import AoC

parseInput :: Parser [Int]
parseInput = many $ decimal <* eol

partOne :: [Int] -> [Int]
partOne = map product . filter ((==2020) . sum) . combinationsOfTwo
  where combinationsOfTwo xs = [ [x,y] | (x:ys) <- tails xs, y <- ys ]

partTwo :: [Int] -> [Int]
partTwo = map product . filter ((==2020) . sum) . combinationsOfThree
  where combinationsOfThree xs = [ [x,y,z] | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs ]

