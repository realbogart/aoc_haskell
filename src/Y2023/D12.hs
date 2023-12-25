module Y2023.D12 where

import AoC

default (Int, Text)

partOneTests = [("???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1", 21)]

data SpringCondition = Unknown | Operational | Damaged
  deriving (Show)

data SpringRecord = SpringRecord
  { conditions :: [SpringCondition]
  , damageGroups :: [Int]
  } deriving (Show)

parseInput :: Parser [SpringRecord]
parseInput = parseLineSeparated parseSpringRecord
  where parseSpringRecord = do
          cs <- some (choice [Unknown <$ char '?', Operational <$ char '.', Damaged <$ char '#']) <* space
          dg <- some (decimal <* optional  (char ','))
          return $ SpringRecord cs dg

partOne _ = 54
