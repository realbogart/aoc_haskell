module Y2021.D2 where

import AoC

default (Text, Int)

data Direction = Up | Down | Forward
  deriving (Show)

data Command = Command
  { direction :: Direction
  , units :: Int
  } deriving (Show)

data Position = Position
  { horizontal :: Int
  , depth :: Int
  , aim :: Int 
  } deriving (Show)

parseInput = parseLineSeparated parseCommand
  where parseCommand = do          
         d <- choice [Up <$ string "up", Down <$ string "down", Forward <$ string "forward"]
         _ <- hspace1
         u <- decimal
         return $ Command d u

input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2" 

partOneTests = [(input, 150)]
partTwoTests = [(input, 900)]

partOne :: [Command] -> Int
partOne = (\p -> horizontal p * depth p) . foldl' move (Position 0 0 0)
  where move :: Position -> Command -> Position
        move (Position h d a) (Command Up u)      = Position h (d - u) a
        move (Position h d a) (Command Down u)    = Position h (d + u) a
        move (Position h d a) (Command Forward u) = Position (h + u) d a

partTwo :: [Command] -> Int
partTwo = (\p -> horizontal p * depth p) . foldl' move (Position 0 0 0)
  where move :: Position -> Command -> Position
        move (Position h d a) (Command Up u)      = Position h d (a - u)
        move (Position h d a) (Command Down u)    = Position h d (a + u)
        move (Position h d a) (Command Forward u) = Position (h + u) (d + a * u) a

