module Y2019.D3 where

import AoC

default (Text, Int)

data Direction = DirLeft | DirRight | DirUp | DirDown
  deriving (Show)

data Jump = Jump
  { dir :: Direction
  , distance :: Int
  } deriving (Show)

data Vec2 = Vec2
  { x :: Int
  , y :: Int
  } deriving (Show, Eq)

data Line = Line
  { a :: Vec2
  , b :: Vec2
  } deriving (Show)

isEqualOrBetween :: Int -> Int -> Int -> Bool
isEqualOrBetween n t1 t2 = n >= min t1 t2 && n <= max t1 t2

getIntersection :: Line -> Line -> Maybe Vec2
getIntersection (Line (Vec2 ax1 ay1) (Vec2 ax2 ay2)) (Line (Vec2 bx1 by1) (Vec2 bx2 by2))
  | isEqualOrBetween bx1 ax1 ax2 && isEqualOrBetween ay1 by1 by2 = Just (Vec2 bx1 ay1) 
  | isEqualOrBetween ax1 bx1 bx2 && isEqualOrBetween by1 ay1 ay2 = Just (Vec2 ax1 by1)
  | otherwise = Nothing

getAllIntersectionsForLine :: Line -> [Line] -> [Vec2]
getAllIntersectionsForLine l = mapMaybe (getIntersection l)

parseInput :: Parser [[Jump]]
parseInput = parseLineSeparated (some parseJump)
  where parseDirection = choice [ DirLeft <$ char 'L'
                                , DirRight <$ char 'R'
                                , DirUp <$ char 'U'
                                , DirDown <$ char 'D' ]
        parseJump = do  d <- parseDirection
                        dist <- decimal
                        _ <- optional (char ',')
                        return $ Jump d dist

partOneTests = [("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", 159),
                ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135),
                ("R8,U5,L5,D3\nU7,R6,D4,L4", 6)]

partTwoTests = [("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", 610),
                ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410),
                ("R8,U5,L5,D3\nU7,R6,D4,L4", 30)]

getVec2FromJump :: Vec2 -> Jump -> Vec2
getVec2FromJump (Vec2 ax ay) (Jump DirRight n) = Vec2 (ax + n) ay 
getVec2FromJump (Vec2 ax ay) (Jump DirLeft n) = Vec2 (ax - n) ay 
getVec2FromJump (Vec2 ax ay) (Jump DirUp n) = Vec2 ax (ay - n)
getVec2FromJump (Vec2 ax ay) (Jump DirDown n) = Vec2 ax (ay + n)

getLineFromJump :: Vec2 -> Jump -> Line
getLineFromJump pos_a jmp = Line pos_a (getVec2FromJump pos_a jmp)

getLines :: [Jump] -> [Line]
getLines = scanl' (getLineFromJump . (\(Line _ b) -> b)) (Line (Vec2 0 0) (Vec2 0 0)) 

getWireIntersections :: [Line] -> [Line] -> [Vec2]
getWireIntersections wiresA wiresB = concatMap (`getAllIntersectionsForLine` wiresB) wiresA

getWires = unwrapWires . map getLines
  where unwrapWires [wiresA, wiresB] = (wiresA, wiresB)
        unwrapWires _ = error "Invalid wires"

getAllIntersecions = filter (/= Vec2 0 0) . uncurry getWireIntersections

vec2toVec2Dist (Vec2 ax ay) (Vec2 bx by) = abs (ax - bx) + abs (ay - by)
vec2Dist = vec2toVec2Dist (Vec2 0 0)

stepsToIntersection :: Int -> [Line] -> Vec2 -> Int
stepsToIntersection _ [] _ = error "No intersection found"
stepsToIntersection acc ((Line la lb):ls) p = 
  case  intersection of 
        Just i -> acc + vec2toVec2Dist i la 
        Nothing -> stepsToIntersection (acc + vec2toVec2Dist la lb) ls p 
  where intersection = getIntersection (Line la lb) (Line p p) 

partOne = minimum . map vec2Dist . getAllIntersecions . getWires

partTwo jumps = minimum $ map (\i -> stepsToIntersection 0 wireA i + stepsToIntersection 0 wireB i) intersections
  where (wireA, wireB) = getWires jumps
        intersections = getAllIntersecions (wireA, wireB)

