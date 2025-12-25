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

distance :: Box -> Box -> Double
distance (ax, ay, az) (bx, by, bz) = sqrt $ fromIntegral $ (ax - bx) ^ 2 + (ay - by) ^ 2 + (az - bz) ^ 2

closest :: [Box] -> [((Box, Box), Double)]
closest boxes = sortBy (\(_, a) (_, b) -> compare a b) (zip combinations distances)
  where
    combinations = comb2 boxes
    distances = map (uncurry distance) combinations

connect :: Clusters -> (Box, Box) -> Clusters
connect clusters (a, b) = new_cluster : next_clusters
  where
    cluster_with_a = concat $ filter (elem a) clusters
    cluster_with_b = concat $ filter (elem b) clusters
    clusters_without_a_and_b = filter (\c -> not (a `elem` c || b `elem` c)) clusters
    combined_clusters = cluster_with_a ++ cluster_with_b
    has_cluster_with_a = (not . null) cluster_with_a
    has_cluster_with_b = (not . null) cluster_with_b
    (new_cluster, next_clusters)
      | null combined_clusters = ([a, b], clusters_without_a_and_b)
      | has_cluster_with_a && has_cluster_with_b && cluster_with_a == cluster_with_b = ([], clusters)
      | has_cluster_with_a && has_cluster_with_b = (combined_clusters, clusters_without_a_and_b)
      | not has_cluster_with_a = (a : cluster_with_b, clusters_without_a_and_b)
      | otherwise = (b : cluster_with_a, clusters_without_a_and_b)

partOne :: [Box] -> Int
partOne boxes = product $ take 3 cluster_sizes
  where
    closest_boxes = map fst $ take 1000 (closest boxes)
    clusters = foldl' connect [] closest_boxes
    cluster_sizes = sortBy (comparing Data.Ord.Down) $ map length clusters
