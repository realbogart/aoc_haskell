module Y2023.D7 where

import AoC

default (Int, Text)

partOneTests = [("32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483", 6440)]

data Hand = Hand
  { cards :: [Char]
  , bid :: Int
  } deriving (Show)

data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard
  deriving (Show)

data HandInfo = HandRank
  { hand_type :: HandType
  , card_values :: [Int]
  } deriving (Show)

getCardValue :: Char -> Int
getCardValue 'A' = 14
getCardValue 'K' = 13
getCardValue 'Q' = 12
getCardValue 'J' = 11
getCardValue c = digitToInt c

getHandType :: Hand -> HandType
getHandType hand = case card_groups of 
                      (5:_) ->    FiveOfAKind
                      (4:3:_) ->  FullHouse
                      (4:_) ->    FourOfAKind
                      (3:_) ->    ThreeOfAKind
                      (2:2:_) ->  TwoPair
                      (2:_) ->    OnePair
                      _ ->        HighCard
  where cards_sorted = sort hand.cards
        card_groups = sortBy (comparing Down) $ map length $ group cards_sorted

parseInput = parseLineSeparated parseHand
  where parseHand = do
          cards <- count 5 latin1Char <* space
          bid <- decimal
          return $ Hand cards bid

partOne :: [Hand] -> Int
partOne _ = 54
