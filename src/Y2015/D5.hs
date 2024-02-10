module Y2015.D5 where

import AoC

default (Text, Int)

partOneTests = [("ugknbfddgicrmopn\naaa\njchzalrnumimnmhp\nhaegwjzuvuyypxyu\ndvszwmarrgswjxmb", 2)]

parseInput :: Parser [[Char]]
parseInput = parseLineSeparated (some letterChar)

isNice :: [Char] -> Bool
isNice s = (length vowels >= 3) && twice && not prohibited
  where
    vowels = filter (`elem` ("aeiou" :: [Char])) s
    twice = any ((> 1) . length) (group s)
    prohibited =  isInfixOf "ab" s ||
                  isInfixOf "cd" s ||
                  isInfixOf "pq" s ||
                  isInfixOf "xy" s

partOne :: [[Char]] -> Int
partOne = length . filter isNice
