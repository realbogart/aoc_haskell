module Main (main) where

import AoC
import Y2025.D8

main :: IO ()
main = do
  parseTestAndSolve parseInput partOne partOneTests "src/Y2025/D8.txt"
