module Y2025.D6 where

import AoC
import Data.List (foldl1', transpose)

default (Text, Int)

type Math = ([([Int], Char)], [([Int], Char)])

parseInput :: Parser Math
parseInput = do
  rows <- parseLineSeparated (some parseInteger <* space) <* optional eol
  operations <- some ((char '*' <|> char '+') <* space)
  let cols = transpose rows
      part1 = zip cols operations
  return (part1, part1)

getOperation :: Char -> (Int -> Int -> Int)
getOperation '*' = (*)
getOperation '+' = (+)
getOperation _ = error "Invalid operation"

evaluate :: ([Int], Char) -> Int
evaluate (numbers, operation) = foldl1' (getOperation operation) numbers

partOneTests = [("123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ", 4277556)]

partTwoTests = [("123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ", 3263827)]

partOne :: Math -> Int
partOne (a, _) = sum $ map evaluate a

partTwo :: Math -> Int
partTwo (_, b) = sum $ map evaluate b
