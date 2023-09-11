module Y2016.D1 where

import AoC
import Y2015.D1 (partOneTests)

data Turn = TurnLeft | TurnRight deriving (Show)
data Move = Move 
  { turn    :: Turn
  , steps   :: Int
  } deriving (Show)

data Vec2 = Vec2
  { x :: Int
  , y :: Int
  } deriving (Show)

type Dir = Vec2
type Pos = Vec2

data Line = Line
  { a :: Vec2
  , b :: Vec2
  } deriving (Show)

turnDir :: Turn -> Dir -> Dir
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

partOneTests :: [(Text,Int)]
partOneTests = [("R2, L3",5), ("R2, R2, R2",2), ("R5, L5, R5, R3", 12)]

partTwoTests :: [(Text,Maybe Int)]
partTwoTests = [("R8, R4, R4, R8",Just 4)]

manhattanToOrigo (Vec2 x y) = abs x + abs y 

partOne :: [Move] -> Int
partOne = manhattanToOrigo . fst . foldl' turnAndWalk (Vec2 0 0, Vec2 0 1)
  where turnAndWalk :: (Pos, Dir) -> Move -> (Pos, Dir) 
        turnAndWalk (Vec2 px py, d) m = (Vec2 (px + ndx * s) (py + ndy * s), Vec2 ndx ndy)
                                          where (Vec2 ndx ndy) = turnDir (turn m) d
                                                s = steps m

getWalkLine :: (Dir,Pos) -> Move -> (Dir,Line)
getWalkLine (d,Vec2 px py) m = (Vec2 ndx ndy, Line (Vec2 (px + ndx) (py + ndy)) (Vec2 (px + ndx * s) (py + ndy * s)))
  where (Vec2 ndx ndy) = turnDir (turn m) d 
        s = steps m

isEqualOrBetween :: Int -> Int -> Int -> Bool
isEqualOrBetween n t1 t2 = n >= min t1 t2 && n <= max t1 t2

getIntersection :: Line -> Line -> Maybe Pos
getIntersection (Line (Vec2 ax1 ay1) (Vec2 ax2 ay2)) (Line (Vec2 bx1 by1) (Vec2 bx2 by2))
  | isEqualOrBetween bx1 ax1 ax2 && isEqualOrBetween ay1 by1 by2 = Just (Vec2 bx1 ay1) 
  | isEqualOrBetween ax1 bx1 bx2 && isEqualOrBetween by1 ay1 ay2 = Just (Vec2 ax1 by1)
  | otherwise = Nothing

getAllIntersectionsForLine :: Line -> [Line] -> [Maybe Pos]
getAllIntersectionsForLine l = filter isJust . map (getIntersection l)

getAllIntersections :: [Line] -> [[Maybe Pos]]
getAllIntersections lines = filter (not . null) $ zipWith getAllIntersectionsForLine lines (inits lines)

getFirstIntersection :: [Line] -> Maybe Pos
getFirstIntersection = getFirstValid . getAllIntersections
  where getFirstValid :: [[Maybe Pos]] -> Maybe Pos 
        getFirstValid [] = Nothing
        getFirstValid (i:is) = head i

partTwo :: [Move] -> Maybe Int
partTwo moves = manhattanToOrigo <$> (getFirstIntersection . map snd . scanl' (getWalkLine . f) (Vec2 0 1, Line (Vec2 0 0) (Vec2 0 0)) $ moves)
  where f (d, Line _ p2) = (d, p2)
