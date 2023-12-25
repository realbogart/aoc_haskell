module Y2023.D10 where

import AoC

default (Int, Text)

partOneTests =  [ (".....\n.S-7.\n.|.|.\n.L-J.\n.....", 4)
                , ("..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ...", 8)]

parseInput :: Parser (Grid Char)
parseInput = getGrid '\n' <$> some latin1Char

findStart :: Grid Char -> GridCoord
findStart g = snd $ head $ filter (\(v, _) -> v == 'S') (zip (map (getGridValue g) indices) indices)
  where horizontalIndices = [0..(g.width - 1)]
        verticalIndices = [0..(g.height - 1)]
        indices = [(x, y) | x <- horizontalIndices, y <- verticalIndices]

stepGrid :: Grid Char -> GridCoord -> GridCoord -> [GridCoord] -> [GridCoord]
stepGrid g (lx, ly) (cx, cy) acc =  case currentTile of
                                      '|' | cy > ly   ->  stepGrid g c (cx, cy + 1) nextAcc
                                          | otherwise ->  stepGrid g c (cx, cy - 1) nextAcc
                                      '-' | cx > lx   ->  stepGrid g c (cx + 1, cy) nextAcc
                                          | otherwise ->  stepGrid g c (cx - 1, cy) nextAcc
                                      'J' | cx > lx   ->  stepGrid g c (cx, cy - 1) nextAcc 
                                          | otherwise ->  stepGrid g c (cx - 1, cy) nextAcc
                                      '7' | cx > lx   ->  stepGrid g c (cx, cy + 1) nextAcc 
                                          | otherwise ->  stepGrid g c (cx - 1, cy) nextAcc
                                      'F' | cy < ly   ->  stepGrid g c (cx + 1, cy) nextAcc
                                          | otherwise ->  stepGrid g c (cx, cy + 1) nextAcc
                                      'L' | cy > ly   ->  stepGrid g c (cx + 1, cy) nextAcc 
                                          | otherwise ->  stepGrid g c (cx, cy - 1) nextAcc
                                      'S' | c == l    ->  stepGrid g c firstMove nextAcc
                                          | otherwise ->  acc
                                      _ -> error "Invalid tile"
  where c = (cx, cy) 
        l = (lx, ly)
        nextAcc = c : acc
        currentTile = getGridValue g c
        rightTile   = getGridValue g (cx + 1, cy)
        belowTile   = getGridValue g (cx, cy + 1)
        leftTile    = getGridValue g (cx - 1, cy)
        aboveTile   = getGridValue g (cx, cy - 1)
        firstMove | rightTile == '-' || rightTile == 'J' = (cx + 1, cy)
                  | belowTile == '|' || belowTile == 'J' = (cx, cy + 1)
                  | leftTile  == '-' || leftTile  == 'F' = (cx - 1, cy)
                  | aboveTile == '|' || aboveTile == 'F' = (cx, cy - 1)
                  | otherwise = error "No valid start move"
        
partOne :: Grid Char -> Int
partOne g = pathLength `div` 2
  where start = findStart g
        path = stepGrid g start start [] 
        pathLength = length path

