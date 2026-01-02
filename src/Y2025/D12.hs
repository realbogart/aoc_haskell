module Y2025.D12 where

import AoC
import Data.Vector qualified as V

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

partOne :: Input -> Int
partOne _ = 5
