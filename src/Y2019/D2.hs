module Y2019.D2 where

import AoC

default (Text, Int)

parseInput = some xs
  where xs :: Parser Int 
        xs = do x <- decimal
                _ <- optional (char ',')
                return x

partOneTests = [("1,9,10,3,2,3,11,0,99,30,40,50",3500)]

partOne :: [Int] -> Int
partOne _ = 654
