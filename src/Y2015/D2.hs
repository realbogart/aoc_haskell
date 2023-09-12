module Y2015.D2 where

import AoC

data Present = Present
  { l :: Int
  , w :: Int
  , h :: Int
  }

parseInput :: Parser [Present]
parseInput = many (present <* optional eol)
  where present :: Parser Present
        present = do
          l <- decimal
          _ <- char 'x'
          w <- decimal
          _ <- char 'x'
          h <- decimal
          return $ Present l w h

partOneTests :: [(Text,Int)]
partOneTests = [("2x3x4",58),("1x1x10",43)]

partTwoTests :: [(Text,Int)]
partTwoTests = [("2x3x4",34),("1x1x10",14)]

getSides :: Present -> [Int]
getSides p = [l p, w p, h p]

getAreas :: Present -> [Int]
getAreas p = [l p * w p, w p * h p, h p * l p] 

getPaper :: Present -> Int
getPaper p = (minimum areas) + sum (map (*2) areas)
  where areas = getAreas p

partOne :: [Present] -> Int
partOne = sum . map getPaper

partTwo :: [Present] -> Int
partTwo = sum . map (getRibbon . getSides)
  where getRibbon sides = shortest sides + product sides
        shortest = sum . map (*2) . tail . reverse . sort 
