module Y2016.D2 where

import AoC

default (Text, Int)

data Direction = NumLeft | NumRight | NumUp | NumDown

parseInput = parseLineSeparated segment
  where segment = some (NumLeft <$ char 'L' <|>
                        NumRight <$ char 'R' <|>
                        NumUp <$ char 'U' <|>
                        NumDown <$ char 'D')

partOneTests = [("ULL\nRRDDD\nLURDL\nUUUUD",[1,9,8,5])]

selectDir :: (a, a, a, a) -> Direction -> a
selectDir (l, _, _, _) NumLeft = l
selectDir (_, r, _, _) NumRight = r
selectDir (_, _, u, _) NumUp = u
selectDir (_, _, _, d) NumDown = d

getNext 1 d = selectDir (1, 2, 1, 4) d
getNext 2 d = selectDir (1, 3, 2, 5) d
getNext 3 d = selectDir (2, 3, 3, 6) d
getNext 4 d = selectDir (4, 5, 1, 7) d
getNext 5 d = selectDir (4, 6, 2, 8) d
getNext 6 d = selectDir (5, 6, 3, 9) d
getNext 7 d = selectDir (7, 8, 4, 7) d
getNext 8 d = selectDir (7, 9, 5, 8) d
getNext 9 d = selectDir (8, 9, 6, 9) d

partOne = tail . scanl getNumber 5 
  where getNumber :: Int -> [Direction] -> Int
        getNumber n = foldl' getNext n 

