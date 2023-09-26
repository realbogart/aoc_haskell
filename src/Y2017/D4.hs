module Y2017.D4 where

import AoC
import Data.Set qualified as S
import Data.List (permutations)

default (Text, Int)

parseInput = parseLineSeparated (some (some letterChar <* hspace))

partOneTests = [("aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa", 2)]
partTwoTests = [("abcde fghij\nabcde xyz ecdab\na ab abc abd abf abj\niiii oiii ooii oooi oooo\noiii ioii iioi iiio", 3)]

hasNoDuplicates :: [[Char]] -> Bool
hasNoDuplicates l = length l == length (S.toList $ S.fromList l)

partOne = length . filter hasNoDuplicates 

partTwo = length . filter hasNoDuplicates . map addPerms
  where addPerms :: [[Char]] -> [[Char]]
        addPerms = concatMap (S.toList . S.fromList . permutations)

