module Y2025.D1 where

import AoC

default (Text, Int)

parseInput :: Parser [Int]
parseInput = parseLineSeparated $ do
  dir :: Int <- (-1 <$ char 'L') <|> (1 <$ char 'R')
  amount <- parseInteger
  return (dir * amount)

partOneTests =
  [ ("L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n", 3)
  ]

partTwoTests =
  [ ("L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n", 6),
    ("L151", 2),
    ("R251", 3)
  ]

turndial :: Int -> Int -> Int
turndial from offset
  | next > 0 = mod next 100
  | otherwise = mod (100 + next) 100
  where
    next = from + offset

passZero :: Int -> Int -> Int
passZero input start =
  -- trace ((show start) ++ ":" ++ (show offset) ++ ":" ++ (show full_laps)) $
  if (offset <= 0 || offset >= 100) && (start /= 0)
    then 1 + full_laps
    else full_laps
  where
    full_laps = div (abs input) 100
    leftover =
      if input > 0
        then input + (-full_laps * 100)
        else input + (full_laps * 100)
    offset = start + leftover

partOne :: [Int] -> Int
partOne = length . filter (== 0) . scanl turndial 50

partTwo :: [Int] -> Int
partTwo input = sum $ outcomes
  where
    after_turns = tail $ scanl turndial 50 input
    pairs = zip input ([50] ++ after_turns)
    outcomes = map (uncurry passZero) $ pairs
