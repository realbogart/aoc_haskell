module Y2017.D3 where

import AoC

default (Text, Int)

parseInput :: Parser Int
parseInput = decimal

partOneTests = [("1", 0), ("12", 3), ("23", 2), ("1024", 31)]

pows = map (\x -> x * x) [1, 3..]

getLayer n = fst . head . dropWhile (\(_, p) -> n > p) $ zip [0..] pows

getLayerCycle :: Int -> [Int]
getLayerCycle n = drop (n - 1) $ cycle $ pattern ++ drop 1 (reverse (drop 1 pattern))
  where pattern = [n..(n + n)]

partOne n = getLayerCycle (getLayer n) !! n 

