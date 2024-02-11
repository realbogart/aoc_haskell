module Y2023.D5 where

import AoC

default (Text, Int)

input = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"

partOneTests = [(input, 35)]

partTwoTests = [(input, 46)]

data Range = Range
  { destination :: Int,
    source :: Int,
    count :: Int
  }
  deriving (Show, Eq, Ord)

data Map = Map
  { name :: [Char],
    ranges :: [Range]
  }
  deriving (Show)

data Almanac = Almanac
  { seeds :: [Int],
    maps :: [Map]
  }
  deriving (Show)

parseInput :: Parser Almanac
parseInput = do
  _ <- string "seeds: "
  seeds <- some (decimal <* hspace)
  _ <- space
  maps <- some (parseMap <* space)
  return $ Almanac seeds maps
  where
    parseMap :: Parser Map
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
mapValue v m
  | num_matches == 0 = v
  | num_matches == 1 = (v - range.source) + range.destination
  | otherwise =
      trace
        ("Value: " ++ show v ++ "\nRanges:\n" ++ show matchingRanges)
        error
        "More than one range matches the value."
  where
    inRange r
      | v < r.source || v >= r.source + r.count = False
      | otherwise = True
    matchingRanges = filter inRange m.ranges
    range = head matchingRanges
    num_matches = length matchingRanges

getRangeIntersection :: Range -> Range -> Maybe Range
getRangeIntersection from to
  | from.source + from.count <= to.source = Nothing
  | from.source >= to.source + to.count = Nothing
  | from.source <= to.source =
      Just $
        Range
          0
          to.destination
          (right - to.source)
  | to.source <= from.source =
      Just $
        Range
          0
          (to.destination + (from.source - to.source))
          (right - from.source)
  | otherwise = error "Should not be here"
  where
    right = min (from.source + from.count) (to.source + to.count)

sortRangesBySource :: [Range] -> [Range]
sortRangesBySource = sortBy (\a b -> compare a.source b.source)

getRangeIntersections :: Range -> [Range] -> [Range]
getRangeIntersections target = mapMaybe (getRangeIntersection target)

getRangeGaps :: Range -> [Range] -> [Range]
getRangeGaps target ranges =
  getRangeIntersections
    target
    ( getFirst sorted_ranges
        ++ getLast sorted_ranges
        ++ mapMaybe getGap range_pairs
    )
  where
    sorted_ranges = sortRangesBySource ranges
    range_pairs = zip sorted_ranges (tail sorted_ranges)
    getFirst :: [Range] -> [Range]
    getFirst (f : _)
      | target.source < f.source = [Range target.source target.source (f.source - target.source)]
      | otherwise = []
    getFirst _ = []
    getLast :: [Range] -> [Range]
    getLast [] = []
    getLast rs
      | (target.source + target.count) > l_right =
          [ Range
              l_right
              l_right
              (target.source + target.count - l_right)
          ]
      | otherwise = []
      where
        l = last rs
        l_right = l.source + l.count
    getGap :: (Range, Range) -> Maybe Range
    getGap (a, b)
      | a.source + a.count < b.source =
          Just $
            Range
              (a.source + a.count)
              (a.source + a.count)
              (b.source - (a.source + a.count))
      | otherwise = Nothing

mapRange :: [Range] -> Map -> [Range]
mapRange targets m = concatMap transformValueRange targets
  where
    transformValueRange t =
      getRangeIntersections t m.ranges
        ++ getRangeGaps t m.ranges

applyMaps :: [Map] -> Int -> Int
applyMaps maps v = foldl' mapValue v maps

applyMapsToRange :: [Map] -> [Range] -> [Range]
applyMapsToRange maps ranges = foldl' mapRange ranges maps

getSeedRanges :: [Int] -> [Range]
getSeedRanges = map getSeedRange . chunksOf 2
  where
    getSeedRange [start, c] = Range 0 start c
    getSeedRange _ = error "Invalid seed pair"

partOne :: Almanac -> Int
partOne a = minimum (map (applyMaps a.maps) a.seeds)

partTwo :: Almanac -> Int
partTwo a = (head sorted_mapped_ranges).source
  where
    seed_ranges = getSeedRanges a.seeds
    mapped_ranges = applyMapsToRange a.maps seed_ranges
    sorted_mapped_ranges = sortRangesBySource mapped_ranges
