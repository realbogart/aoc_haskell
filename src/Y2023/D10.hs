module Y2023.D10 where

import AoC

default (Int, Text)

partOneTests =  [ (".....\n.S-7.\n.|.|.\n.L-J.\n.....", 4)
                , ("..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ...", 8)]

-- parseInput = parseLineSeparated (some $ choice  [ char '.', char 'S', char '|'
--                                                 , char '-', char 'J', char 'L'
--                                                 , char 'F', char '7'])

parseInput :: Parser (Grid Char)
parseInput = getGrid '\n' <$> some latin1Char

partOne :: Grid Char -> Int
partOne _ = 54

