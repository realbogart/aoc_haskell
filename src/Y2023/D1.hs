module Y2023.D1 where

import AoC

default (Int, Text)

parseInput = parseLineSeparated (some (letterChar <|> numberChar))

partOneTests = [("1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet", 142)]
partTwoTests = [("two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen", 281)]

firstAndLast l = [head l, last l]

transformNumbers [] = []
transformNumbers cs 
  | "one"   `isPrefixOf` cs = '1' : rest
  | "two"   `isPrefixOf` cs = '2' : rest
  | "three" `isPrefixOf` cs = '3' : rest
  | "four"  `isPrefixOf` cs = '4' : rest
  | "five"  `isPrefixOf` cs = '5' : rest
  | "six"   `isPrefixOf` cs = '6' : rest
  | "seven" `isPrefixOf` cs = '7' : rest
  | "eight" `isPrefixOf` cs = '8' : rest
  | "nine"  `isPrefixOf` cs = '9' : rest
  | otherwise = head cs : rest
  where rest = transformNumbers (tail cs)

solve = sum . map (read . firstAndLast . filter isDigit)

partOne :: [[Char]] -> Int
partOne = solve

partTwo :: [[Char]] -> Int
partTwo = solve . map transformNumbers
