module Y2025.D4 where

import AoC
import Data.Vector qualified as V

default (Text, Int)

parseInput :: Parser (Grid Bool)
parseInput = do
  rows <- parseLineSeparated (some (True <$ char '@' <|> False <$ char '.'))
  let w = length $ head rows
      h = length rows
  return $ Grid (V.fromList (concat rows)) w h

partOneTests = [("..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.", 13)]

canAccess :: Grid Bool -> GridCoord -> Bool
canAccess grid coord = length (filter (getGridValue grid) neighbours) < 4
  where
    neighbours = getGridNeighbours grid coord

getPaperCoords :: Grid Bool -> [GridCoord]
getPaperCoords grid = [(x, y) | x <- [0 .. (grid.width - 1)], y <- [0 .. (grid.height - 1)], getGridValue grid (x, y)]

partOne :: Grid Bool -> Int
partOne grid = length $ filter (canAccess grid) $ getPaperCoords grid
