module Y2018.D1 where

import AoC
import qualified Data.Set as Set

default (Text, Int)

parseInput = parseLineSeparated frequencyShift
    where frequencyShift = do
            sign <- (-1) <$ char '-' <|> 1 <$ char '+'
            frequency <- decimal
            delim <- choice [void $ string ", ", void eol, eof]
            return $ sign * frequency

partOneTests = [("+1, +1, +1", 3),("+1, +1, -2", 0),("-1, -2, -3", (-6))]
partTwoTests = [("+1, -1", 0), ("+3, +3, +4, -2, -4", 10), ("-6, +3, +8, +5, -6", 5),
                ("+7, +7, -2, -7, -4", 14)]

partOne :: [Int] -> Int
partOne = sum

partTwo :: [Int] -> Int
partTwo input = firstDuplicate Set.empty 0 (cycle input)
  where firstDuplicate previous acc (x:xs)  | Set.member acc previous = acc
                                            | otherwise = firstDuplicate (Set.insert acc previous) (x+acc) xs

