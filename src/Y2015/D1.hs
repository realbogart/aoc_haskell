module Y2015.D1 where

import AoC

parseInput :: Parser [Char]
parseInput = manyTill anySingle eof

partOne :: [Char] -> Int
partOne = sum . map (\c -> if c == '(' then 1 else -1)

