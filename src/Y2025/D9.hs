module Y2025.D9 where

import AoC

default (Text, Int)

type Tile = (Int, Int)

parseInput :: Parser [Tile]
parseInput = parseLineSeparated $ do
  x <- parseInteger <* char ','
  y <- parseInteger
  return (x, y)

partOneTests = [("7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3", 50)]

rectangleSize :: Tile -> Tile -> Int
rectangleSize (ax, ay) (bx, by) = (abs (ax - bx) + 1) * (abs (ay - by) + 1)

partOne :: [Tile] -> Int
partOne tiles = maximum sizes
  where
    pairs = comb2 tiles
    sizes = map (uncurry rectangleSize) pairs
