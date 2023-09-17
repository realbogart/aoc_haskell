module Y2020.D2 where

import AoC

default (Text, Int)

data PasswordEntry = PasswordEntry
  { low :: Int
  , high :: Int
  , letter :: Char
  , password :: [Char]
  } deriving (Show)

parseInput = parseLineSeparated parseEntry
  where parseEntry :: Parser PasswordEntry
        parseEntry = do
          low <- decimal
          _ <- char '-'
          high <- decimal
          _ <- hspace1
          letter <- letterChar
          _ <- string ": "
          password <- some letterChar
          return $ PasswordEntry low high letter password

partOneTests = [("1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc", 2)]
partTwoTests = [("1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc", 1)]

valid1 e = occurrences >= low e && occurrences <= high e 
  where occurrences = length $ filter (== letter e) $ password e

valid2 e = (a /= b) && ((a == l) || (b == l)) 
  where p = password e
        a = p !! (low e - 1)
        b = p !! (high e - 1)
        l = letter e

partOne = length . filter valid1
partTwo = length . filter valid2

