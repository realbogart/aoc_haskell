module Y2025.D8 where

import AoC
import Data.Ord

default (Text, Int)

type Box = (Int, Int, Int)

type Clusters = [[Box]]

parseInput :: Parser [Box]
parseInput = parseLineSeparated parseBox
  where
    parseBox :: Parser Box
    parseBox = do
      x <- parseInteger <* char ','
      y <- parseInteger <* char ','
      z <- parseInteger
      return (x, y, z)

-- partOneTests =
--   [ ("162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689", 40)
--   ]

partOneTests = []

partTwoTests =
  [ ("162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689", 25272)
  ]

distance :: Box -> Box -> Double
distance (ax, ay, az) (bx, by, bz) = sqrt $ fromIntegral $ (ax - bx) ^ 2 + (ay - by) ^ 2 + (az - bz) ^ 2

closest :: [Box] -> [((Box, Box), Double)]
closest boxes = sortBy (\(_, a) (_, b) -> compare a b) (zip combinations distances)
  where
    combinations = comb2 boxes
    distances = map (uncurry distance) combinations

connect :: Clusters -> (Box, Box) -> Clusters
connect clusters (a, b) = next_clusters
  where
    cluster_with_a = find (elem a) clusters
    cluster_with_b = find (elem b) clusters
    clusters_without_a_and_b = filter (\c -> not (a `elem` c || b `elem` c)) clusters
    next_clusters
      | Nothing <- cluster_with_a, Nothing <- cluster_with_b = [a, b] : clusters
      | Just ca <- cluster_with_a, Just cb <- cluster_with_b, ca == cb = clusters
      | Just ca <- cluster_with_a, Just cb <- cluster_with_b = (ca ++ cb) : clusters_without_a_and_b
      | Nothing <- cluster_with_a, Just cb <- cluster_with_b = (a : cb) : clusters_without_a_and_b
      | Just ca <- cluster_with_a, Nothing <- cluster_with_b = (b : ca) : clusters_without_a_and_b

initClusters :: [Box] -> Clusters
initClusters = map (: [])

partOne :: [Box] -> Int
partOne boxes = product $ take 3 cluster_sizes
  where
    start_clusters = initClusters boxes
    closest_boxes = map fst $ closest boxes
    clusters = foldl' connect start_clusters $ take 1000 closest_boxes
    cluster_sizes = sortBy (comparing Data.Ord.Down) $ map length clusters

partTwo :: [Box] -> Int
partTwo boxes = last_a_x * last_b_x
  where
    start_clusters = initClusters boxes
    closest_boxes = map fst $ closest boxes
    clusters = scanl connect start_clusters $ take 4000 closest_boxes
    num_clusters = map length clusters
    boxes_num_clusters = filter (\(_, n) -> n == 1) (zip closest_boxes (tail num_clusters))
    (((last_a_x, _, _), (last_b_x, _, _)), _) = head boxes_num_clusters
