module Y2025.D6 where

import AoC
import Data.List (foldl1', transpose)

default (Text, Int)

type Math = [([Int], Char)]

-- This is the parser for part 1
--
-- parseInput :: Parser Math
-- parseInput = do
--   rows <- parseLineSeparated (some parseInteger <* space) <* optional eol
--   operations <- some ((char '*' <|> char '+') <* space)
--   let cols = transpose rows
--   return $ zip cols operations

parseInput :: Parser Math
parseInput = do
  rows <- parseLineSeparated (some (digitChar <|> char ' '))
  operations_row <- some (char '*' <|> char '+' <|> char ' ')
  let cols = transpose rows
      number_cols = map (filter isDigit) cols
      number_cols_split = splitOn [""] number_cols
      numbers = map (map getNumber) number_cols_split
      operations = filter (/= ' ') operations_row
  return $ zip numbers operations

getNumber :: [Char] -> Int
getNumber = read . filter isDigit

getOperation :: Char -> (Int -> Int -> Int)
getOperation '*' = (*)
getOperation '+' = (+)
getOperation _ = error "Invalid operation"

evaluate :: ([Int], Char) -> Int
evaluate (numbers, operation) = foldl1' (getOperation operation) numbers

partOneTests = [("123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ", 4277556)]

partTwoTests = [("123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ", 3263827)]

partOne :: Math -> Int
partOne = sum . map evaluate

partTwo = partOne
