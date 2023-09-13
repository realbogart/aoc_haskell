module Y2016.D2 where

import AoC

data Direction = NumLeft | NumRight | NumUp | NumDown

parseInput = parseLineSeparated segment
  where segment = some (NumLeft <$ char 'L' <|>
                        NumRight <$ char 'R' <|>
                        NumUp <$ char 'U' <|>
                        NumDown <$ char 'D')

partOneTests = [("ULL\nRRDDD\nLURDL\nUUUUD",[1,9,8,5])]

getNext :: Int -> Direction -> Int
getNext 1 NumRight = 2
getNext 1 NumDown = 4
getNext 2 NumLeft = 1
getNext 2 NumRight = 3
getNext 2 NumDown = 5
getNext 3 NumLeft = 2
getNext 3 NumDown = 6
getNext 4 NumUp = 1
getNext 4 NumRight = 5
getNext 4 NumDown = 7
getNext 5 NumLeft = 4
getNext 5 NumUp = 2
getNext 5 NumRight = 6
getNext 5 NumDown = 8
getNext 6 NumLeft = 5
getNext 6 NumUp = 3
getNext 6 NumDown = 9
getNext 7 NumUp = 4
getNext 7 NumRight = 8
getNext 8 NumLeft = 7
getNext 8 NumUp = 5
getNext 8 NumRight = 9
getNext 9 NumLeft = 8
getNext 9 NumUp = 6
getNext x _ = x

-- getCode :: (a -> Direction -> a) -> a -> [[Direction]] -> [a]
-- getCode f n ds = scanl getNumber n ds 
--   where getNumber nn dd = foldl' getNext nn dd
--
-- partOne :: [[Direction]] -> [Int]
-- partOne = getCode getNext 5 

partOne = tail . scanl getNumber 5 
  where getNumber :: Int -> [Direction] -> Int
        getNumber n = foldl' getNext n 
