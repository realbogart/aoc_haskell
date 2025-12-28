module Y2025.D10 where

import AoC
import Data.Bits

default (Text, Int)

type Button = Int

type Indicators = Int

type Joltage = Int

data Machine = Machine
  { indicators_target :: Indicators,
    buttons :: [Button],
    joltage :: [Joltage]
  }
  deriving (Show)

indicatorBits :: [Char] -> Indicators
indicatorBits cs = foldl' (\acc (i, c) -> if c == '#' then setBit acc i else acc) 0 indices
  where
    indices = zip [0 .. length cs - 1] cs

buttonBits :: [Int] -> Button
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

input = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

partOneTests =
  [ ("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}", 7)
  ]

pressButton :: Indicators -> Button -> Indicators
pressButton = xor

pressButtons :: [Button] -> Indicators -> [Indicators]
pressButtons buttons indicators = map (pressButton indicators) buttons

nextIndicators :: [Button] -> [Indicators] -> [Indicators]
nextIndicators buttons = concatMap (pressButtons buttons)

findLeastPresses_ :: [Button] -> Indicators -> [Indicators] -> Int -> Int
findLeastPresses_ buttons indicators_target all_indicators presses
  | indicators_target `elem` all_indicators_after_presses = presses + 1
  | otherwise = findLeastPresses_ buttons indicators_target all_indicators_after_presses (presses + 1)
  where
    all_indicators_after_presses = nextIndicators buttons all_indicators

findLeastPresses :: Machine -> Int
findLeastPresses (Machine indicators_target buttons _) = findLeastPresses_ buttons indicators_target [0] 0

partOne :: [Machine] -> Int
partOne = sum . map findLeastPresses
