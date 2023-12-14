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

data HandType = EmptyHand | HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
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

getCardValueJoker :: Char -> Int
getCardValueJoker 'J' = 1
getCardValueJoker c = getCardValue c

getHandType :: Hand -> HandType
getHandType hand = case card_groups of 
                      (5:_) ->    FiveOfAKind
                      (4:_) ->    FourOfAKind
                      (3:2:_) ->  FullHouse
                      (3:_) ->    ThreeOfAKind
                      (2:2:_) ->  TwoPair
                      (2:_) ->    OnePair
                      [] ->       EmptyHand
                      _ ->        HighCard
  where cards_sorted = sort hand.cards
        card_groups = sortBy (comparing Down) $ map length $ group cards_sorted

upgradeHandTypeWithJoker EmptyHand = HighCard
upgradeHandTypeWithJoker HighCard = OnePair
upgradeHandTypeWithJoker OnePair = ThreeOfAKind
upgradeHandTypeWithJoker TwoPair = FullHouse
upgradeHandTypeWithJoker ThreeOfAKind = FourOfAKind
upgradeHandTypeWithJoker FullHouse = error "Can't have a full house and a joker at the same time!"
upgradeHandTypeWithJoker FourOfAKind = FiveOfAKind
upgradeHandTypeWithJoker FiveOfAKind = error "Can't have five of a kind and a joker at the same time!"

getHandTypeJoker :: Hand -> HandType
getHandTypeJoker hand = last $ take (number_of_jokers + 1) $ iterate upgradeHandTypeWithJoker base_hand_type
  where cards_without_jokers = filter (/= 'J') hand.cards
        number_of_jokers = 5 - length cards_without_jokers
        base_hand_type = getHandType (Hand cards_without_jokers 0)

getHandInfo :: Hand -> HandInfo
getHandInfo hand = HandInfo (getHandType hand) (map getCardValue hand.cards)

getHandInfoJoker :: Hand -> HandInfo
getHandInfoJoker hand = HandInfo (getHandTypeJoker hand) (map getCardValueJoker hand.cards)

parseInput = parseLineSeparated parseHand
  where parseHand = do
          cs <- count 5 latin1Char <* space
          Hand cs <$> decimal

ranks = [1..]

partOne :: [Hand] -> Int
partOne hands = sum $ zipWith (*) (map bid hands_sorted) ranks
  where hands_sorted = sortBy (comparing getHandInfo) hands

partTwo :: [Hand] -> Int
partTwo hands = sum $ zipWith (*) (map bid hands_sorted) ranks
  where hands_sorted = sortBy (comparing getHandInfoJoker) hands
