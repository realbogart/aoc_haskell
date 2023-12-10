module Y2023.D5 where

import AoC

default (Text, Int)

input = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"

partOneTests = [(input, 35)]

data Range = Range
  { destination :: Int
  , source :: Int
  , count :: Int
  } deriving (Show)

data Map = Map
  { name :: [Char] 
  , ranges :: [Range]
  } deriving (Show)

data Almanac = Almanac
  { seeds :: [Int]
  , maps :: [Map]
  } deriving (Show)

parseInput :: Parser Almanac
parseInput = do
  _ <- string "seeds: "
  seeds <- some (decimal <* hspace)
  _ <- space
  maps <- some (parseMap <* space)
  return $ Almanac seeds maps
  where parseMap :: Parser Map
        parseMap = do
          name <- some (letterChar <|> char '-') <* string " map:" <* space
          ranges <- parseLineSeparated parseRange
          return $ Map name ranges
        parseRange :: Parser Range
        parseRange = do
          d <- decimal <* hspace
          s <- decimal <* hspace
          c <- decimal <* space
          return $ Range d s c

mapValue :: Int -> Map -> Int
mapValue v m  | num_matches == 0 = v 
              | num_matches == 1 = (v - range.source) + range.destination
              | otherwise = trace ("Value: " ++ show v ++ "\nRanges:\n" ++ show matchingRanges)
                            error "More than one range matches the value."
  where inRange r | v < r.source || v >= r.source + r.count = False
                  | otherwise = True
        matchingRanges = filter inRange m.ranges
        range = head matchingRanges
        num_matches = length matchingRanges

applyMaps :: [Map] -> Int -> Int
applyMaps maps v = foldl' mapValue v maps

partOne :: Almanac -> Int
partOne a = minimum (map (applyMaps a.maps) a.seeds)
