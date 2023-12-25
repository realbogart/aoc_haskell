module Y2023.D10 where

import AoC

default (Int, Text)

partOneTests =  [ (".....\n.S-7.\n.|.|.\n.L-J.\n.....", 4)
                , ("..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ...", 8)]

partTwoTests = [ ("...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n...........", 4), (".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ...", 10)]

parseInput :: Parser (Grid Char)
parseInput = newGridFromList '\n' <$> some latin1Char

findStart :: Grid Char -> GridCoord
findStart g = snd $ head $ filter (\(v, _) -> v == 'S') (zip (map (getGridValue g) indices) indices)
  where horizontalIndices = [0..(g.width - 1)]
        verticalIndices = [0..(g.height - 1)]
        indices = [(x, y) | x <- horizontalIndices, y <- verticalIndices]

stepGrid :: Grid Char -> GridCoord -> GridCoord -> [GridCoord] -> [GridCoord]
stepGrid g (lx, ly) (cx, cy) acc = -- trace (show c) $ 
  case currentTile of
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

findMainLoop :: Grid Char -> [GridCoord]
findMainLoop g = mainLoop
  where start = findStart g
        mainLoop = stepGrid g start start []

partOne :: Grid Char -> Int
partOne g = length mainLoop `div` 2
  where mainLoop = findMainLoop g

partTwo :: Grid Char -> Int
partTwo _ = 54
