module Y2025.D10 where

import AoC
import Data.Bits

default (Text, Int)

data Machine = Machine
  { indicators :: Int,
    buttons :: [Int],
    joltage :: [Int]
  }
  deriving (Show)

indicatorBits :: [Char] -> Int
indicatorBits cs = foldl' (\acc (i, c) -> if c == '#' then setBit acc i else acc) 0 indices
  where
    indices = zip [0 .. length cs - 1] cs

buttonBits :: [Int] -> Int
buttonBits = foldl' setBit 0

parseMachine :: Parser Machine
parseMachine = do
  indicators <- indicatorBits <$> (char '[' *> (some (char '.' <|> char '#') <* string "] "))
  buttons <- some parseButton
  joltage <- parseJoltage
  return $ Machine indicators buttons joltage
  where
    parseButton :: Parser Int
    parseButton = buttonBits <$> (char '(' *> some (parseInteger <* optional (char ',')) <* string ") ")

    parseJoltage :: Parser [Int]
    parseJoltage = char '{' *> some (parseInteger <* optional (char ',')) <* char '}'

parseInput :: Parser [Machine]
parseInput = parseLineSeparated parseMachine

partOneTests =
  [ ("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}", 7)
  ]

partOne :: [Machine] -> Int
partOne _ = 5
