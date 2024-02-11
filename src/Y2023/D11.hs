module Y2023.D11 where

import AoC

default (Int, Text)

input = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."

partOneTests = [(input, 374)]

partTwoTests = []

parseInput :: Parser (Grid Char)
parseInput = newGridFromList '\n' <$> some latin1Char

expandVertical n cs row
  | any (\(_, y) -> y == row) cs = cs
  | otherwise = map f cs
  where
    f (cx, cy)
      | cy > row = (cx, cy + n)
      | otherwise = (cx, cy)

expandHorizontal n cs col
  | any (\(x, _) -> x == col) cs = cs
  | otherwise = map f cs
  where
    f (cx, cy)
      | cx > col = (cx + n, cy)
      | otherwise = (cx, cy)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

getExpandedDistances :: Int -> Grid Char -> Int
getExpandedDistances n g = sum $ map (uncurry distance) galaxyCombinations
  where
    galaxyCoords = findGridCoords g (== '#')
    galaxyCoordsExpandedVertical = foldl' (expandVertical n) galaxyCoords (reverse [0 .. (g.height - 1)])
    galaxyCoordsExpandedAll = foldl' (expandHorizontal n) galaxyCoordsExpandedVertical (reverse [0 .. (g.width - 1)])
    combinationsOfTwo cs = [(x, y) | (x : ys) <- tails cs, y <- ys]
    galaxyCombinations = combinationsOfTwo galaxyCoordsExpandedAll

partOne = getExpandedDistances 1

partTwo = getExpandedDistances 999999
