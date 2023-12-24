module Y2017.D3 where

import AoC

import Data.Vector qualified as V

default (Text, Int)

parseInput :: Parser Int
parseInput = decimal

partOneTests = [("1", 0), ("12", 3), ("23", 2), ("1024", 31)]
partTwoTests = [("760", 806)]

pows = map (\x -> x * x) [1, 3..]

getLayer n = fst . head . dropWhile (\(_, p) -> n > p) $ zip [0..] pows

getLayerCycle :: Int -> [Int]
getLayerCycle n = drop (n - 1) $ cycle $ pattern ++ drop 1 (reverse (drop 1 pattern))
  where pattern = [n..(n + n)]

partOne n = getLayerCycle (getLayer n) !! n 

getSpiralCoords :: (Int, Int) -> Int -> [(Int, Int)]
getSpiralCoords (sx, sy) n = (sx, sy) : (sx + 1, sy) : up ++ left ++ down ++ right ++ getSpiralCoords (sx + 1, sy - 1) (n + 2)
  where up = [(sx + 1, y) | y <- [(sy + 1)..(sy + n)]]
        left = [(x, sy + n) | x <- reverse [(sx - n)..sx]]
        down = [(sx - n, y) | y <- reverse [(sy - 1)..sy + n - 1]]
        right = [(x, sy - 1) | x <- [(sx - n + 1)..sx]]

getIndex (x, y) = py * 100 + px
  where (px, py) = (x + 50, y + 50) 

getGridValueTmp grid p = grid V.! getIndex p
setGridValue grid p v = V.update grid (V.fromList [(getIndex p, v)]) 

getNeighbours (x, y) = [(x + 1, y), (x + 1, y + 1), 
                        (x, y + 1), (x - 1, y + 1), 
                        (x - 1, y), (x - 1, y - 1), 
                        (x, y - 1), (x + 1, y - 1)]

walkSpiral grid (p:ps) v | nextValue > v = nextValue
                         | otherwise = walkSpiral (setGridValue grid p nextValue) ps v
  where nextValue = sum $ map (getGridValueTmp grid) (getNeighbours p)
walkSpiral  _ _ _ = error "Something went wrong"

partTwo :: Int -> Int
partTwo = walkSpiral grid (drop 1 $ getSpiralCoords (0, 0) 1)
  where grid = setGridValue (V.fromList (replicate 10000 0)) (0,0) 1 
