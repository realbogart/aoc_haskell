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
partTwo = pack . removePred (/=) . head . filter ((== 1) . length . removePred (==)) . comb2
  where removePred f (a:as,b:bs)  | f a b = removePred f (as,bs)
                                  | otherwise = a : removePred f (as,bs)
        removePred _ _ = []

