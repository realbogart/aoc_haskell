module Y2025.D5 where

import AoC

default (Text, Int)

type FreshRange = (Int, Int)

type Ingredient = Int

data Database
  = Database
  { ranges :: [FreshRange],
    available :: [Ingredient]
  }
  deriving (Show)

parseInput :: Parser Database
parseInput = do
  ranges <- some parseRange
  _ <- space
  available <- parseLineSeparated parseInteger
  return $ Database ranges available
  where
    parseRange :: Parser FreshRange
    parseRange = do
      from <- parseInteger <* char '-'
      to <- parseInteger <* optional eol
      return (from, to)

partOneTests = [("3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32", 3)]

inRange :: Ingredient -> FreshRange -> Bool
inRange ingredient (start, end) = ingredient >= start && ingredient <= end

inAnyRange :: [FreshRange] -> Ingredient -> Bool
inAnyRange ranges ingredient = any (inRange ingredient) ranges

partOne :: Database -> Int
partOne database = length $ filter (inAnyRange database.ranges) database.available
