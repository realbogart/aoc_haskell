module Y2015.D1 where

import AoC
import Data.Text qualified as T

default (Text, Int)

parseInput :: Parser T.Text
parseInput = T.pack <$> manyTill anySingle eof

partOneTests =
  [ ("(())", 0),
    ("()()", 0),
    ("(((", 3),
    ("(()(()(", 3),
    ("))(((((", 3),
    ("())", -1),
    ("())", -1),
    (")))", -3),
    (")())())", -3)
  ]

partTwoTests = [(")", 1), ("()())", 5)]

partOne :: T.Text -> Int
partOne = sum . map (\c -> if c == '(' then 1 else -1) . T.unpack

partTwo :: T.Text -> Int
partTwo input = basementPosition 0 0 (T.unpack input)
  where
    basementPosition p _ [] = p
    basementPosition p cfloor (c : cs)
      | cfloor == -1 = p
      | otherwise = basementPosition (p + 1) (cfloor + (if c == '(' then 1 else -1)) cs
