module Y2015.D3 where

import AoC
import Data.Set

default (Text, Int)

data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

data Direction = North | South | East | West
  deriving (Show)

parseInput :: Parser [Direction]
parseInput = some directions
  where
    directions = choice [North <$ char '^', South <$ char 'v', East <$ char '>', West <$ char '<']

partOneTests = [(">", 2), ("^>v<", 4), ("^v^v^v^v^v", 2)]

partTwoTests = [("^v", 3), ("^>v<", 3), ("^v^v^v^v^v", 11)]

move (Position x y) North = Position x (y + 1)
move (Position x y) South = Position x (y - 1)
move (Position x y) East = Position (x + 1) y
move (Position x y) West = Position (x - 1) y

getVisits = scanl' move (Position 0 0)

partOne = length . toList . fromList . getVisits

partTwo dirs = length . toList . fromList $ (getVisits (santaDirs dirs) ++ getVisits (roboDirs dirs))
  where
    santaDirs [] = []
    santaDirs (x : xs) = x : roboDirs xs
    roboDirs [] = []
    roboDirs (_ : xs) = santaDirs xs
