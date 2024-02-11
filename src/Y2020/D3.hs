module Y2020.D3 where

import AoC

default (Text, Int)

parseInput = parseLineSeparated parseTreeLine
  where
    parseTreeLine = some (True <$ char '#' <|> False <$ char '.')

input = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

partOneTests = [(input, 7)]

partTwoTests = [(input, 336)]

treeLine terrain y1 = cycle (terrain !! y1)

cell terrain x y = treeLine terrain y !! x

getSlopes terrain =
  [ ([1 ..], [1 .. h]),
    ([3, 6 ..], [1 .. h]),
    ([5, 10 ..], [1 .. h]),
    ([7, 14 ..], [1 .. h]),
    ([1 ..], [2, 4 .. h])
  ]
  where
    h = length terrain - 1

getCollisions terrain (slope_dx, slope_dy) = length $ filter id (zipWith (cell terrain) slope_dx slope_dy)

partOne terrain = getCollisions terrain (getSlopes terrain !! 1)

partTwo terrain = product $ map (getCollisions terrain) (getSlopes terrain)
