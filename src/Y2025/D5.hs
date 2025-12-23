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

partTwoTests = [("3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32", 14)]

inRange :: Ingredient -> FreshRange -> Bool
inRange ingredient (start, end) = ingredient >= start && ingredient <= end

inAnyRange :: [FreshRange] -> Ingredient -> Bool
inAnyRange ranges ingredient = any (inRange ingredient) ranges

isOverlap :: FreshRange -> FreshRange -> Bool
isOverlap (start_a, end_a) (start_b, end_b) = end_a >= start_b && start_a <= end_b

getMergedRange :: FreshRange -> FreshRange -> FreshRange
getMergedRange a@(start_a, end_a) b@(start_b, end_b)
  | isOverlap a b = (min start_a start_b, max end_a end_b)
  | otherwise = b

mergeOverlapping :: [FreshRange] -> FreshRange
mergeOverlapping = foldl1 getMergedRange

mergeAll :: [FreshRange] -> [FreshRange]
mergeAll ranges
  | ranges == ranges_next = ranges
  | otherwise = mergeAll ranges_next
  where
    groups = groupBy isOverlap ranges
    ranges_next = map (foldl1 getMergedRange) groups

countRange :: FreshRange -> Int
countRange (start, end) = 1 + (end - start)

partOne :: Database -> Int
partOne database = length $ filter (inAnyRange database.ranges) database.available

partTwo :: Database -> Int
partTwo database = sum counts
  where
    ordered_ranges = sortBy (\(start_a, _) (start_b, _) -> compare start_a start_b) database.ranges
    merged = mergeAll ordered_ranges
    counts = map countRange merged
