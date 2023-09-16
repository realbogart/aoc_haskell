module Y2018.D2 where

import AoC

default (Text, Int)

parseInput = parseLineSeparated (some letterChar)
partOneTests = [("abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab", 12)]
partTwoTests = [("abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz", "fgij")]

partOne boxes = (length . filter id . map twos) boxes * (length . filter id . map threes) boxes
  where twos = elem 2 . map length . group . sort
        threes = elem 3 . map length . group . sort 

partTwo :: [[Char]] -> Text
partTwo = pack . removeDiffering . head . filter ((==1) . length . removeMatching) . comb2
  where removeMatching (a:as,b:bs)  | a == b = removeMatching (as,bs)
                                    | otherwise = a : removeMatching (as,bs)
        removeMatching _ = []
        removeDiffering (a:as,b:bs) | a == b = a : removeDiffering (as,bs)
                                    | otherwise = removeDiffering (as,bs)
        removeDiffering _ = []
