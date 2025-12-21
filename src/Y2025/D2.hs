module Y2025.D2 where

import AoC

default (Text, Int)

parseInput :: Parser [(Int, Int)]
parseInput = many parseIds
  where
    parseIds :: Parser (Int, Int)
    parseIds = do
      a <- parseInteger <* char '-'
      b <- parseInteger <* optional (char ',')
      return (a, b)

partOneTests =
  [ ("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124", 1227775554),
    ("111-112", 0)
  ]

partTwoTests =
  [ ("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124", 4174379265)
  ]

allIds :: [(Int, Int)] -> [Int]
allIds = concatMap (\(a, b) -> [a .. b])

hasRepeats :: String -> Int -> Bool
hasRepeats str n = any (uncurry (==)) pairs
  where
    chunks = chunksOf n str
    pairs = zip chunks (tail chunks)

allRepeats :: String -> Int -> Bool
allRepeats str n = all (== first_chunk) chunks
  where
    chunks = chunksOf n str
    first_chunk = head chunks

validId :: Int -> Bool
validId x = not (any (hasRepeats str) check_chunks) || odd len
  where
    str = show x
    len = length str
    check_chunks = [div (length str) 2]

validId2 :: Int -> Bool
validId2 x = not $ any (allRepeats str) check_chunks
  where
    str = show x
    check_chunks = [1 .. div (length str) 2]

partOne :: [(Int, Int)] -> Int
partOne = sum . filter (not . validId) . allIds

partTwo :: [(Int, Int)] -> Int
partTwo = sum . filter (not . validId2) . allIds
