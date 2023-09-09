module Y2020.D1 where

import AoC

import Data.List

import Text.Megaparsec (many)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

integerListParser :: Parser [Int]
integerListParser = many $ decimal <* eol

d1 :: [Int] -> [Int]
d1 = map product . filter ((==2020) . sum) . combinationsOfTwo
  where combinationsOfTwo xs = [ [x,y] | (x:ys) <- tails xs, y <- ys ]

d1_2 :: [Int] -> [Int]
d1_2 = map product . filter ((==2020) . sum) . combinationsOfThree
  where combinationsOfThree xs = [ [x,y,z] | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs ]

