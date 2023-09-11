module Y2015.D1 where

import AoC
import qualified Data.Text as T

parseInput :: Parser T.Text
parseInput = T.pack <$> manyTill anySingle eof

partOneTests :: [(T.Text, Int)]
partOneTests = [("(())",0), ("()()",0), ("(((",3), ("(()(()(",3),
                ("))(((((",3), ("())",(-1)), ("())",(-1)), (")))",(-3)), (")())())",(-3))]

partTwoTests :: [(T.Text, Int)]
partTwoTests = [(")",1), ("()())",5)]

partOne :: T.Text -> Int
partOne = sum . map (\c -> if c == '(' then 1 else -1) . T.unpack

partTwo :: T.Text -> Int
partTwo input = basementPosition 0 0 (T.unpack input)
  where basementPosition p _ [] = p 
        basementPosition p floor (c:cs) | floor == -1 = p
                                        | otherwise = basementPosition (p+1) (floor + (if c == '(' then 1 else -1)) cs
