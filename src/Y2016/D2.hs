module Y2016.D2 where

import AoC

default (Text, Int)

data Direction = NumLeft | NumRight | NumUp | NumDown
  deriving (Show)

parseInput = parseLineSeparated segment
  where segment = some (NumLeft <$ char 'L' <|>
                        NumRight <$ char 'R' <|>
                        NumUp <$ char 'U' <|>
                        NumDown <$ char 'D')

partOneTests = [("ULL\nRRDDD\nLURDL\nUUUUD",[1,9,8,5])]
partTwoTests = [("ULL\nRRDDD\nLURDL\nUUUUD",['5','D','B','3'])]

selectDir :: (a, a, a, a) -> Direction -> a
selectDir (l, _, _, _) NumLeft = l
selectDir (_, r, _, _) NumRight = r
selectDir (_, _, u, _) NumUp = u
selectDir (_, _, _, d) NumDown = d

-- 1 2 3
-- 4 5 6
-- 7 8 9

getNext 1 = selectDir (1, 2, 1, 4)
getNext 2 = selectDir (1, 3, 2, 5)
getNext 3 = selectDir (2, 3, 3, 6)
getNext 4 = selectDir (4, 5, 1, 7)
getNext 5 = selectDir (4, 6, 2, 8)
getNext 6 = selectDir (5, 6, 3, 9)
getNext 7 = selectDir (7, 8, 4, 7)
getNext 8 = selectDir (7, 9, 5, 8)
getNext 9 = selectDir (8, 9, 6, 9)

--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D

getNext2 '1' = selectDir ('1', '1', '1', '3')
getNext2 '2' = selectDir ('1', '3', '2', '6')
getNext2 '3' = selectDir ('2', '4', '1', '7')
getNext2 '4' = selectDir ('3', '4', '4', '8')
getNext2 '5' = selectDir ('5', '6', '5', '5')
getNext2 '6' = selectDir ('5', '7', '2', 'A')
getNext2 '7' = selectDir ('6', '8', '3', 'B')
getNext2 '8' = selectDir ('7', '9', '4', 'C')
getNext2 '9' = selectDir ('8', '9', '9', '9')
getNext2 'A' = selectDir ('A', 'B', '6', 'C')
getNext2 'B' = selectDir ('A', 'C', '7', 'D')
getNext2 'C' = selectDir ('B', 'C', '8', 'C')
getNext2 'D' = selectDir ('D', 'D', 'B', 'D')

getCode :: (a -> Direction -> a) -> a -> [[Direction]] -> [a]
getCode nextFn n = tail . scanl getNumber n 
  where getNumber = foldl' nextFn

partOne = getCode getNext 5
partTwo = getCode getNext2 '5'

