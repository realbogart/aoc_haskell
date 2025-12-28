module Main (main) where

import AoC
import Y2025.D9

main :: IO ()
main = do
  parseTestAndSolve parseInput partTwo partTwoTests "src/Y2025/D9.txt"
