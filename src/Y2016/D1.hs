module Y2016.D1 where

import AoC

data Turn = TurnLeft | TurnRight deriving (Show)
data Move = Move 
  { turn    :: Turn
  , steps   :: Int
  } deriving (Show)

data Vec2 = Vec2
  { x :: Int
  , y :: Int
  } deriving (Show)

data Line = Line
  { a :: Vec2
  , b :: Vec2
  } deriving (Show)

turnDir :: Turn -> Vec2 -> Vec2
turnDir TurnLeft (Vec2 x y) = Vec2 (-y) x
turnDir TurnRight (Vec2 x y) = Vec2 y (-x)

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
partOne = manhattanToOrigo . fst . foldl' turnAndWalk (Vec2 0 0, Vec2 0 1)
  where turnAndWalk :: (Vec2, Vec2) -> Move -> (Vec2, Vec2) 
        turnAndWalk (Vec2 px py, d) m = (Vec2 (px + ndx * s) (py + ndy * s), Vec2 ndx ndy)
                                          where (Vec2 ndx ndy) = turnDir (turn m) d
                                                s = steps m
        manhattanToOrigo (Vec2 x y) = abs x + abs y 

getWalkLine :: (Vec2,Vec2) -> Move -> (Vec2,Line)
getWalkLine (d,Vec2 px py) m = (Vec2 ndx ndy, Line (Vec2 (px + ndx) (py + ndy)) (Vec2 (px + ndx * s) (py + ndy * s)))
  where (Vec2 ndx ndy) = turnDir (turn m) d 
        s = steps m

partTwo :: [Move] -> [(Vec2,Line)]
partTwo = tail . scanl' (getWalkLine . f) (Vec2 0 1, Line (Vec2 0 0) (Vec2 0 0))
  where f (d, Line _ p2) = (d, p2)
