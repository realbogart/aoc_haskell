module Y2021.D2 where

import AoC

default (Text, Int)

data Direction = DirUp | DirDown | DirForward
  deriving (Show)

data Command = Command
  { direction :: Direction,
    units :: Int
  }
  deriving (Show)

data Position = Position
  { horizontal :: Int,
    depth :: Int,
    aim :: Int
  }
  deriving (Show)

parseInput = parseLineSeparated parseCommand
  where
    parseCommand = do
      d <- choice [DirUp <$ string "up", DirDown <$ string "down", DirForward <$ string "forward"]
      _ <- hspace1
      u <- decimal
      return $ Command d u

input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

partOneTests = [(input, 150)]

partTwoTests = [(input, 900)]

partOne :: [Command] -> Int
partOne = (\p -> p.horizontal * p.depth) . foldl' move (Position 0 0 0)
  where
    move :: Position -> Command -> Position
    move (Position h d a) (Command DirUp u) = Position h (d - u) a
    move (Position h d a) (Command DirDown u) = Position h (d + u) a
    move (Position h d a) (Command DirForward u) = Position (h + u) d a

partTwo :: [Command] -> Int
partTwo = (\p -> p.horizontal * p.depth) . foldl' move (Position 0 0 0)
  where
    move :: Position -> Command -> Position
    move (Position h d a) (Command DirUp u) = Position h d (a - u)
    move (Position h d a) (Command DirDown u) = Position h d (a + u)
    move (Position h d a) (Command DirForward u) = Position (h + u) (d + a * u) a
