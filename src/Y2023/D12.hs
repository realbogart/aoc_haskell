module Y2023.D12 where

import AoC
import Data.Vector qualified as V

default (Int, Text)

partOneTests = [("???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1", 21)]

data SpringCondition = Unknown | Operational | Damaged
  deriving (Show, Eq)

data SpringRecord = SpringRecord
  { conditions :: V.Vector SpringCondition
  , damageGroups :: [Int]
  } deriving (Show)

parseInput :: Parser [SpringRecord]
parseInput = parseLineSeparated parseSpringRecord
  where parseSpringRecord = do
          cs <- V.fromList <$> some (choice [Unknown <$ char '?', Operational <$ char '.', Damaged <$ char '#']) <* space
          dg <- some (decimal <* optional  (char ','))
          return $ SpringRecord cs dg

validGroup :: Int -> [SpringCondition] -> Bool
validGroup size []  | size == 0 = True
                    | otherwise = False
validGroup size [c] | size == 0 = c == Unknown || c == Operational
validGroup size (c:rest)  | c == Unknown || c == Damaged = validGroup (size - 1) rest
                          | otherwise = False

countSlots :: [Int] -> V.Vector SpringCondition -> Int
countSlots [] _ = 0
countSlots (size:dgs_rest) scs  | null dgs_rest = if is_first_damaged then 1 else length valid_groups
                                | is_first_damaged && 
                                  validGroup size (V.toList first_damaged_group) = countSlots dgs_rest rest
                                | otherwise = trace (show valid_groups) $ sum $ map (countSlots dgs_rest . snd) valid_groups
  where first_damaged = V.findIndex (== Damaged) scs
        (first_damaged_group, rest) = V.splitAt (min (size + 1) (V.length scs)) scs
        is_first_damaged = case first_damaged of
                            Nothing -> False
                            Just fd -> fd == 0
        test_slots = case first_damaged of
                      Nothing -> scs
                      Just i -> V.slice 0 i scs
        test_positions = [0..(V.length test_slots - size - 1)]
        test_start = map (snd . (`V.splitAt` scs)) test_positions
        test_groups = map (V.splitAt (size + 1)) test_start
        valid_groups = filter (validGroup size . V.toList . fst) test_groups

partOne :: [SpringRecord] -> Int
partOne srs = countSlots (head test).damageGroups (head test).conditions 
  where test = tail srs
