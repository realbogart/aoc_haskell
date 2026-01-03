module Y2025.D12 where

import AoC
import Data.Function ((&))
import Data.List (nub, transpose)
import Data.Vector qualified as V
import GHC.IO (unsafePerformIO)

default (Text, Int)

type Shape = V.Vector Bool

data Region = Region
  { width :: Int,
    height :: Int,
    shape_ids :: V.Vector Int
  }
  deriving (Show)

data Input = Input
  { shapes :: V.Vector Shape,
    regions :: [Region]
  }
  deriving (Show)

parseShape :: Parser Shape
parseShape = do
  _ <- parseInteger <* char ':' <* space
  V.fromList <$> count 9 ((True <$ char '#' <|> False <$ char '.') <* optional space)

parseRegion :: Parser Region
parseRegion = do
  width <- parseInteger <* char 'x'
  height <- parseInteger <* char ':' <* space
  shape_ids <- V.fromList <$> some parseInteger
  return $ Region width height shape_ids

parseInput :: Parser Input
parseInput = do
  shapes <- V.fromList <$> count 6 parseShape
  regions <- parseLineSeparated parseRegion
  return $ Input shapes regions

partOneTests =
  [ ("0:\n###\n##.\n##.\n\n1:\n###\n##.\n.##\n\n2:\n.##\n###\n##.\n\n3:\n##.\n###\n##.\n\n4:\n###\n#..\n###\n\n5:\n###\n.#.\n###\n\n4x4: 0 0 0 0 2 0\n12x5: 1 0 1 0 2 2\n12x5: 1 0 1 0 3 2", 2)
  ]

variants :: Shape -> V.Vector Shape
variants s = V.uniq $ V.fromList $ nub $ s : map V.fromList [flipped_h, flipped_v, flipped, trans, trans_flipped_h, trans_flipped_v, trans_reversed]
  where
    l = V.toList s
    flipped = reverse l
    l3 = chunksOf 3 l
    trans = transpose l3 & concat
    trans_reversed = reverse trans
    trans3 = chunksOf 3 trans
    flipped_h = l3 & concatMap reverse
    flipped_v = l3 & reverse & concat
    trans_flipped_h = trans3 & concatMap reverse
    trans_flipped_v = trans3 & reverse & concat

printShape :: Shape -> IO ()
printShape s = do
  mapM_
    ( \indices -> do
        mapM_ (\i -> printTile (s V.! i)) indices
        putStrLn ""
    )
    $ chunksOf 3 [0 .. 8]
  putStrLn ""
  where
    printTile t = if t then putChar '#' else putChar '.'

partOne :: Input -> Int
partOne (Input shapes regions) = unsafePerformIO $ do
  let all_variants = V.concatMap variants shapes
  mapM_ printShape all_variants
  return 5
