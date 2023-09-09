module Y2015.D1 where

import AoC

parseInput :: Parser [Char]
parseInput = manyTill anySingle eof

partOne :: [Char] -> Int
partOne = sum . map (\c -> if c == '(' then 1 else -1)

partTwo :: [Char] -> Int
partTwo input = basementPosition 0 0 (init input)
  where basementPosition p _ [] = p 
        basementPosition p floor (c:cs) | floor == -1 = p
                                        | otherwise = basementPosition (p+1) (floor + (if c == '(' then 1 else -1)) cs
