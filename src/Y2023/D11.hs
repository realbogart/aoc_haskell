module Y2023.D11 where

import AoC

default (Int, Text)

input = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."
partOneTests = [(input, 374)]

parseInput :: Parser (Grid Char)
parseInput = newGridFromList '\n' <$> some latin1Char

-- expandVertical cs row | any (\(_, y) -> y == row) cs = trace ("No expand for " ++ show row) cs
--                       | otherwise = trace ("Expanding " ++ show row) $ map f cs
--   where f (cx, cy)  | cy > row = trace ("Changing " ++ show (cx, cy) ++ " to " ++ show (cx, cy + 1)) (cx, cy + 1)
--                     | otherwise = trace ("Not expanding " ++ show (cx, cy)) (cx, cy)

expandVertical cs row | any (\(_, y) -> y == row) cs = cs
                      | otherwise = map f cs
  where f (cx, cy)  | cy > row = (cx, cy + 1)
                    | otherwise = (cx, cy)

expandHorizontal cs col | any (\(x, _) -> x == col) cs = cs
                        | otherwise = map f cs
  where f (cx, cy)  | cx > col = (cx + 1, cy)
                    | otherwise = (cx, cy)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

partOne :: Grid Char -> Int
partOne g = sum $ map (uncurry distance) galaxyCombinations
  where galaxyCoords = findGridCoords g (== '#')
        galaxyCoordsExpandedVertical = foldl' expandVertical galaxyCoords (reverse [0..(g.height - 1)])
        galaxyCoordsExpandedAll = foldl' expandHorizontal galaxyCoordsExpandedVertical (reverse [0..(g.width - 1)])
        combinationsOfTwo cs = [ (x,y) | (x:ys) <- tails cs, y <- ys ]
        galaxyCombinations = combinationsOfTwo galaxyCoordsExpandedAll

-- [(0,2),(0,9),(1,5),(3,0),(4,9),(6,4),(7,1),(7,8),(9,6)]

