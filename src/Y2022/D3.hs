module Y2022.D3 where

import AoC

default (Text, Int)
parseInput = parseLineSeparated (some letterChar)

input = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw" 

partOneTests = [(input, 157)]
partTwoTests = [(input, 70)]

getCompartments rucksack = splitAt (div (length rucksack) 2) rucksack
findCommon (a, b) = head $ filter (`elem` b) a 

getPriority c | isUpper c = subtract 38 . ord $ c
              | otherwise = subtract 96 . ord $ c

findCommon3 :: [[Char]] -> Char
findCommon3 [a,b,c] = findCommon (common_a_b, c)
  where common_a_b = filter (`elem` b) a
findCommon3 _ = error "Invalid input"

partOne = sum . map (getPriority . findCommon . getCompartments)
partTwo = sum . map (getPriority . findCommon3) . chunksOf 3
