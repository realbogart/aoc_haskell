module Y2017.D1 where

import AoC

parseInput :: Parser [Int]
parseInput = map digitToInt <$> many digitChar

partOne :: [Int] -> Int
partOne (x:xs) = sum . map (sum . tail) . filter ((>1) . length) . group $ ((x:xs) ++ [x])
