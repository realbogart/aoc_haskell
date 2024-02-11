module Y2016.D3 where

import AoC

default (Text, Int)

data Triangle = Triangle
  { a :: Int,
    b :: Int,
    c :: Int
  }
  deriving (Show)

parseInput = parseLineSeparated parseTriangle
  where
    parseTriangle = do
      _ <- optional hspace1
      a <- decimal
      _ <- hspace1
      b <- decimal
      _ <- hspace1
      c <- decimal
      return $ Triangle a b c

partOneTests = [("5 10 25", 0)]

partTwoTests = []

validTriangle :: Triangle -> Bool
validTriangle t =
  t.a + t.b > t.c
    && t.a + t.c > t.b
    && t.b + t.c > t.a

countValid f = length . filter id . map f

convertThreeTriangles :: [Triangle] -> [Triangle]
convertThreeTriangles [t1, t2, t3] =
  [ Triangle (t1.a) (t2.a) (t3.a),
    Triangle (t1.b) (t2.b) (t3.b),
    Triangle (t1.c) (t2.c) (t3.c)
  ]
convertThreeTriangles _ = []

convertTriangles = concatMap convertThreeTriangles . chunksOf 3

partOne = countValid validTriangle

partTwo = countValid validTriangle . convertTriangles
