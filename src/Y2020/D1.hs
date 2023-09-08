module Y2020.D1(d1, d1Test) where

import Distribution.Simple.Utils
import Data.List
-- module Main

combinationsOfTwo :: [a] -> [[a]]
combinationsOfTwo xs = [ [x,y] | (x:ys) <- tails xs, y <- ys ]

combinationsOfThree :: [a] -> [[a]]
combinationsOfThree xs = [ [x,y,z] | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs ]

d1 :: String -> IO ()
d1 = do 
  print . map product . filter ((==2020) . sum) . combinationsOfTwo . map read . words

d1_2 :: String -> IO ()
d1_2 = do 
  print . map product . filter ((==2020) . sum) . combinationsOfThree . map read . words

d1Test :: IO ()
d1Test = withFileContents "src/Y2020/d1_example.txt" d1
