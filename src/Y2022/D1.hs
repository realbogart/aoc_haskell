module Y2022.D1 where

import AoC

parseInput :: Parser [[Int]]
parseInput = many (parseBackpack <* optional eol)
  where parseBackpack = some (decimal <* optional eol)

input :: Text
input = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" 

partOneTests :: [(Text, Int)]
partOneTests = [(input,24000)]

partTwoTests :: [(Text, Int)]
partTwoTests = [(input,45000)]

partOne :: [[Int]] -> Int
partOne = maximum . map sum

partTwo :: [[Int]] -> Int
partTwo = sum . take 3 . reverse . sort . map sum

