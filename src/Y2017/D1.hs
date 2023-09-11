module Y2017.D1 where

import AoC

parseInput :: Parser [Int]
parseInput = map digitToInt <$> many digitChar

partOneTests :: [(Text, Int)]
partOneTests = [("1122", 3), ("1111", 4), ("1234", 0), ("91212129", 9)]

partTwoTests :: [(Text, Int)]
partTwoTests = [("1212",6), ("1221",0), ("123425",4), ("123123",12), ("12131415",4)]

partOne :: [Int] -> Int
partOne (x:xs) = sum . map (sum . tail) . filter ((>1) . length) . group $ ((x:xs) ++ [x])

partTwo :: [Int] -> Int
partTwo xs = sum $ map fst $ filter matchingDigit (zip xs (drop jmp xs ++ take jmp xs))
  where matchingDigit (x,y) = x == y
        jmp = div (length xs) 2

