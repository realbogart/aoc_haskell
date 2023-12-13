module Y2023.D6 where

import AoC

default (Int, Text)

input = "Time:      7  15   30\nDistance:  9  40  200"

partOneTests = [(input, 288)]
partTwoTests = [(input, 71503)]

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

getBigRace :: [Race] -> Race
getBigRace = g . foldl' f ("","")
  where f (acc_time, acc_distance) race = (acc_time ++ show race.time, acc_distance ++ show race.distance)
        g (acc_time, acc_distance) = Race (read acc_time) (read acc_distance)

partOne :: [Race] -> Int
partOne = product . map getBrokenRecords

partTwo :: [Race] -> Int
partTwo races = getBrokenRecords big_race
  where big_race = getBigRace races
