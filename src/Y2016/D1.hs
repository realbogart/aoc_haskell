module Y2016.D1 where

import AoC

data Turn = TurnLeft | TurnRight deriving (Show)
data Move = Move 
  { turn    :: Turn
  , steps   :: Int
  } deriving (Show)

turnDir :: Turn -> (Int, Int) -> (Int, Int)
turnDir TurnLeft (x, y) = (-y, x)
turnDir TurnRight (x, y) = (y, -x)

parseMove :: Parser Move
parseMove = do
  turn <- TurnLeft <$ char 'L' <|> TurnRight <$ char 'R'
  steps <- decimal
  delim <- choice [void $ string ", ", eof, void eol]
  return $ Move turn steps

parseInput :: Parser [Move]
parseInput = do
  many parseMove

partOne :: [Move] -> Int
partOne = manhattanToOrigo . fst . foldl' turnAndWalk ((0,0), (0,1))
  where turnAndWalk ((px,py),d) m = ((px + ndx * s, py + ndy * s), (ndx,ndy))
                                          where (ndx,ndy) = turnDir (turn m) d
                                                s = steps m
        manhattanToOrigo = uncurry ((+) `on` abs) 

partTwo :: [Move] -> Int
partTwo = manhattanToOrigo . fst . foldl' turnAndWalk ((0,0), (0,1))
  where turnAndWalk ((px,py),d) m = ((px + ndx * s, py + ndy * s), (ndx,ndy))
                                          where (ndx,ndy) = turnDir (turn m) d
                                                s = steps m
        manhattanToOrigo = uncurry ((+) `on` abs) 
