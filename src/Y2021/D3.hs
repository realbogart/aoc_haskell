module Y2021.D3 where

import AoC

default (Text, Int)
parseInput = parseLineSeparated (some (1 <$ char '1' <|> 0 <$ char '0'))

input = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010" 
partOneTests = [(input, 198)]
partTwoTests = [(input, 230)]

getIndices l = [0..length l - 1]
getColumn l n = map (!! n) l

getMostCommon :: Ord a => [a] -> a
getMostCommon = snd . maximum . map (\l -> (length l, head l)) . group . reverse . sort

invert = map (\x -> if x == 0 then 1 else 0)
toDecimal = sum . zipWith (*) [2 ^ n | n <- [0..]] . reverse

partOne l = toDecimal gamma * toDecimal epsilon
  where gamma = map (getMostCommon . getColumn l) (getIndices $ head l)
        epsilon = invert gamma

partTwo l = 5
  where mc = map (getMostCommon . getColumn l) (getIndices $ head l)
        lc = invert mc

