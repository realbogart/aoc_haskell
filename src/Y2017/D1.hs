module Y2017.D1 where

import AoC

parseInput :: Parser [Int]
parseInput = map digitToInt <$> many digitChar

partOneTests :: [(Text, Int)]
partOneTests = [("1122", 3), ("1111", 4), ("1234", 0), ("91212129", 9)]

partOne :: [Int] -> Int
partOne (x:xs) = sum . map (sum . tail) . filter ((>1) . length) . group $ ((x:xs) ++ [x])
partOne [] = 1337
