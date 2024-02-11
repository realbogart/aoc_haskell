module Y2023.D4 where

import AoC

default (Text, Int)

input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

partOneTests = [(input, 13)]

partTwoTests = [(input, 30)]

data Card = Card
  { card_id :: Int,
    winning_numbers :: [Int],
    your_numbers :: [Int]
  }
  deriving (Show)

parseInput = parseLineSeparated parseCard
  where
    parseCard = do
      card_id <- string "Card" *> hspace *> decimal <* char ':'
      winning_numbers <- some (hspace *> decimal <* hspace)
      _ <- char '|'
      your_numbers <- some (hspace *> decimal)
      return $ Card card_id winning_numbers your_numbers

countMatches c = length $ filter (\n -> n `elem` c.winning_numbers) c.your_numbers

cardScore :: Card -> Int
cardScore c
  | matches < 1 = 0
  | otherwise = 2 ^ (matches - 1)
  where
    matches = countMatches c

countClones :: [Card] -> Int -> Int
countClones cards card_id
  | matches < 1 = 1
  | otherwise = 1 + sum (map (countClones cards) cloned_ids)
  where
    target = cards !! card_id
    matches = countMatches target
    cloned_ids = [target.card_id .. target.card_id + matches - 1]

partOne = sum . map cardScore

partTwo :: [Card] -> Int
partTwo cards = sum $ map (countClones cards) [0 .. length cards - 1]
