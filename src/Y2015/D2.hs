module Y2015.D2 where

import AoC

default (Text, Int)

data Present = Present
  { l :: Int,
    w :: Int,
    h :: Int
  }
  deriving (Show)

parseInput = parseLineSeparated present
  where
    present = do
      l <- decimal
      _ <- char 'x'
      w <- decimal
      _ <- char 'x'
      h <- decimal
      return $ Present l w h

partOneTests = [("2x3x4", 58), ("1x1x10", 43)]

partTwoTests = [("2x3x4", 34), ("1x1x10", 14)]

getSides p = [p.l, p.w, p.h]

getAreas p = [p.l * p.w, p.w * p.h, p.h * p.l]

getPaper p = minimum areas + sum (map (* 2) areas)
  where
    areas = getAreas p

partOne :: [Present] -> Int
partOne = sum . map getPaper

partTwo :: [Present] -> Int
partTwo = sum . map (getRibbon . getSides)
  where
    getRibbon sides = shortest sides + product sides
    shortest = sum . map (* 2) . tail . reverse . sort
