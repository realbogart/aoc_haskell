module Y2023.D12 where

import AoC
import Data.Vector qualified as V

default (Int, Text)

input = "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"

partOneTests = [(input, 21)]
partTwoTests = [(input, 525152)]

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

validGroup :: Int -> Int -> V.Vector SpringCondition -> Bool
validGroup size l scs | l == 0 = size == 0
                      | l == 1 && size == 0 = c == Unknown || c == Operational
                      | c == Unknown || c == Damaged = validGroup (size - 1) (l - 1) (snd $ V.splitAt 1 scs)
                      | otherwise = False
  where c = scs V.! 0

countSlots :: [Int] -> V.Vector SpringCondition -> Int
countSlots [] _ = 0
countSlots (size:dgs_rest) scs  | null dgs_rest = length valid_groups
                                -- | otherwise = trace (show size ++ show valid_groups) $ sum $ map (countSlots dgs_rest . snd) valid_groups
                                | otherwise = sum $ map (countSlots dgs_rest . snd) valid_groups
  where first_damaged = V.findIndex (== Damaged) scs
        remainder_space = length dgs_rest + sum dgs_rest
        test_slots = case first_damaged of
                      Nothing -> scs
                      Just i -> V.slice 0 (min (i + size + 1) (length scs - remainder_space)) scs
        test_positions = [0..(V.length test_slots - size - 1)]
        test_start = map (snd . (`V.splitAt` scs)) test_positions
        test_groups = map (V.splitAt (size + 1)) test_start
        test_groups_and_lengths = map (\(tg,_) -> (length tg, tg)) test_groups
        valid_groups = filter (uncurry (validGroup size)) test_groups_and_lengths

countRecord :: SpringRecord -> Int
-- countRecord sr = trace ("SpringConditions: " ++ show sr.conditions ++ " Slots: " ++ show numSlots) numSlots
countRecord sr = trace (show "Processing...") numSlots
  where split_record = (sr.damageGroups, sr.conditions V.++ V.fromList [Operational])
        numSlots = uncurry countSlots split_record

unfoldRecord :: SpringRecord -> SpringRecord
unfoldRecord r = SpringRecord unfolded_conditions unfolded_damage_groups
  where unfolded_conditions = V.concat $ intersperse (V.fromList [Unknown]) (replicate 5 r.conditions)
        unfolded_damage_groups = concat $ replicate 5 r.damageGroups

partOne :: [SpringRecord] -> Int
partOne = sum . map countRecord

partTwo:: [SpringRecord] -> Int
partTwo = sum . parMap rpar (countRecord . unfoldRecord)
-- partTwo srs = countRecord $ unfoldRecord (srs !! 1)

