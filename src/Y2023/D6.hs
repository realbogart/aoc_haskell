module Y2023.D6 where

import AoC

default (Int, Text)

partOneTests = [("Time:      7  15   30\nDistance:  9  40  200", 288)]

data Race = Race
  { time :: Int
  , distance :: Int 
  } deriving (Show)

parseInput :: Parser [Race]
parseInput = do
  _ <- string "Time:" <* space
  times <- some (decimal <* space)
  _ <- string "Distance:" <* space
  distances <- some (decimal <* space)
  return $ zipWith Race times distances

getDistanceFromButtonTime race bt = movement_time * bt
  where movement_time = max (race.time - bt) 0

beatRecord race distance = distance > race.distance

getBrokenRecords race = length $ filter id $ map (beatRecord race . getDistanceFromButtonTime race) button_durations
  where button_durations = [0..race.time]

partOne :: [Race] -> Int
partOne = product . map getBrokenRecords
