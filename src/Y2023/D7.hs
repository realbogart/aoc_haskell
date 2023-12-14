module Y2023.D7 where

import AoC

default (Int, Text)

input = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

partOneTests = [(input, 6440)]
partTwoTests = [(input, 5905)]

data Hand = Hand
  { cards :: [Char]
  , bid :: Int
  } deriving (Show)

cards hand = hand.cards
bid hand = hand.bid

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Eq, Ord)

data HandInfo = HandInfo
  { hand_type :: HandType
  , card_values :: [Int]
  } deriving (Show, Eq, Ord)

getCardValue :: Char -> Int
getCardValue 'A' = 14
getCardValue 'K' = 13
getCardValue 'Q' = 12
getCardValue 'J' = 11
getCardValue 'T' = 10
getCardValue c = digitToInt c

getHandType :: Hand -> HandType
getHandType hand = case card_groups of 
                      (5:_) ->    FiveOfAKind
                      (4:_) ->    FourOfAKind
                      (3:2:_) ->  FullHouse
                      (3:_) ->    ThreeOfAKind
                      (2:2:_) ->  TwoPair
                      (2:_) ->    OnePair
                      _ ->        HighCard
  where cards_sorted = sort hand.cards
        card_groups = sortBy (comparing Down) $ map length $ group cards_sorted

getHandInfo :: Hand -> HandInfo
getHandInfo hand = HandInfo (getHandType hand) (map getCardValue hand.cards)

parseInput = parseLineSeparated parseHand
  where parseHand = do
          cs <- count 5 latin1Char <* space
          Hand cs <$> decimal

partOne :: [Hand] -> Int
partOne hands = sum $ zipWith (*) (map bid hands_sorted) ranks
  where hands_sorted = sortBy (comparing getHandInfo) hands
        ranks = [1..]

partTwo :: [Hand] -> Int
partTwo _ = 54
