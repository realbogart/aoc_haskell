module Y2018.D4 where

import AoC

default (Text, Int)

data EntryType = ShiftStart | FallAsleep | WakeUp
  deriving (Show)

data Timestamp = Timestamp
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  } deriving (Show)

data EntryData = EntryData
  { entry_type :: EntryType
  , timestamp :: Timestamp
  , guard_id :: Int
  , sleep_from :: Int
  , sleep_to :: Int
  } deriving (Show)

parseTimestamp :: Parser Timestamp
parseTimestamp = do 
  _ <- char '['
  y <- decimal <* char '-'
  mo <- decimal <* char '-'
  d <- decimal <* hspace
  h <- decimal <* char ':'
  m <- decimal
  _ <- char ']'
  return $ Timestamp y mo d h m

parseInput :: Parser [EntryData]
parseInput = parseLineSeparated parseEntry 
  where parseEntry = do
          ets <- parseTimestamp <* hspace
          etype <- choice [ShiftStart <$ string "Guard", FallAsleep <$ string "falls", WakeUp <$ string "wakes"] <* hspace 
          eg :: Int <-  case etype of
                          ShiftStart -> char '#' *> decimal <* some (anySingleBut '\n')
                          _ -> 0 <$ some (anySingleBut '\n')
          return $ EntryData etype ets eg 0 0

partOneTests = [("[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up", 240)]

partOne l = length l :: Int
