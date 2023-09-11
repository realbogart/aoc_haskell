module Y2019.D1 where

import AoC

parseInput :: Parser [Int]
parseInput = many (decimal <* (eol <|> ""))

partOneTests :: [(Text,Int)]
partOneTests = [("12",2),("14",2),("1969",654),("100756",33583)]

partTwoTests :: [(Text,Int)]
partTwoTests = [("14",2),("1969",966),("100756",50346)]

getFuel = subtract 2 . (`div` 3)
getFuel2 m  | fuel <= 0 = 0 
            | otherwise = fuel + getFuel2 fuel
              where fuel = getFuel m

partOne :: [Int] -> Int
partOne = sum . map getFuel

partTwo :: [Int] -> Int
partTwo = sum . map getFuel2
