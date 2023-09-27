module Y2018.D4 where

import AoC
import Data.Vector qualified as V

default (Text, Int)

data EntryType = ShiftStart | FallAsleep | WakeUp
  deriving (Show, Eq, Ord)

data Timestamp = Timestamp
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  } deriving (Show, Eq, Ord)

data EntryData = EntryData
  { timestamp :: Timestamp
  , entry_type :: EntryType
  , guard_id :: Int
  } deriving (Show, Eq, Ord)

data SleepShift = SleepShift
  { sleep_guard_id :: Int
  , sleep_start :: Int 
  , sleep_end :: Int
  } deriving (Show, Eq)

parseTimestamp :: Parser Timestamp
parseTimestamp = do 
  y <- char '[' *> decimal <* char '-'
  mo <- decimal <* char '-'
  d <- decimal <* hspace
  h <- decimal <* char ':'
  m <- decimal <* char ']'
  return $ Timestamp y mo d h m

cleanEntryData :: [EntryData] -> [EntryData]
cleanEntryData = scanl1 copyGuardID . sort
  where copyGuardID _ current@EntryData{entry_type = ShiftStart} = current 
        copyGuardID EntryData{guard_id = gid} current = current{guard_id = gid}

parseInput :: Parser [EntryData]
parseInput = cleanEntryData <$> parseLineSeparated parseEntry 
  where parseEntry = do
          ets <- parseTimestamp <* hspace
          etype <- choice [ShiftStart <$ string "Guard", FallAsleep <$ string "falls", WakeUp <$ string "wakes"] <* hspace 
          eg :: Int <-  case etype of
                          ShiftStart -> char '#' *> decimal <* some (anySingleBut '\n')
                          _ -> 0 <$ some (anySingleBut '\n')
          return $ EntryData ets etype eg

input = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"

partOneTests = [(input, 240)]
partTwoTests = [(input, 4455)]

getSleepEntries = filter (\EntryData{entry_type = et} -> et /= ShiftStart)
getSleepShifts = map getSleepShift . chunksOf 2 . getSleepEntries
  where getSleepShift [sleep,awake] = SleepShift (guard_id sleep) (minute(timestamp sleep)) (minute(timestamp awake))
        getSleepShift _ = error "Corrupt/incomplete sleep entries"

compareGuard a b = compare (sleep_guard_id a) (sleep_guard_id b)
matchingGuard a b = sleep_guard_id a == sleep_guard_id b
groupGuards = groupBy matchingGuard . sortBy compareGuard 

getTotalMinutes :: [SleepShift] -> [(Int, Int)]
getTotalMinutes = map (foldl' addMinutes (0,0)) . groupGuards
  where addMinutes (_, total) SleepShift{sleep_guard_id = gid, sleep_start = s, sleep_end = e} = (gid, total + (e - s))

getBiggestSleeper = maximumBy (\(_,a) (_,b) -> compare a b) . getTotalMinutes

getMostSleptMinuteForID :: Int -> [SleepShift] -> (Int, Int, Int)
getMostSleptMinuteForID gid = idAndMostCommonAndCount . 
                              foldl' logMinutes initMinutes . 
                              filter (\SleepShift{sleep_guard_id = g} -> g == gid)
  where initMinutes = V.fromList (replicate 60 0)
        logMinutes v SleepShift{sleep_start = s, sleep_end = e} = updatedMinutes
          where minutes = init [s..e]
                updatedMinutes = V.update v (V.fromList (zip minutes (map (incIndex v) minutes)))
        incIndex v i = 1 + (v V.! i)
        idAndMostCommonAndCount v = (gid, V.maxIndex v, V.maximum v)

partOne :: [EntryData] -> Int
partOne ed = biggest_sleeper_id * mc
  where sleepShifts = getSleepShifts ed
        (biggest_sleeper_id, _) = getBiggestSleeper sleepShifts
        (_, mc, _) = getMostSleptMinuteForID biggest_sleeper_id sleepShifts
        
-- partTwo :: [EntryData] -> Int
partTwo l = answer $ maximumBy (\(_,_,a) (_,_,b) -> compare a b) $
            map ((`getMostSleptMinuteForID` sleepShifts) . (\SleepShift{sleep_guard_id = gid} -> gid) . head) 
                (groupGuards sleepShifts)
  where sleepShifts = getSleepShifts l
        answer (x,y,_) = x * y
