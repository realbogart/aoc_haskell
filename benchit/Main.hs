module Main (main) where

import AoC
-- import Y2023.D12
import Y2024.D17

main :: IO ()
main = do
  parseTestAndSolve parseInput partTwo partTwoTests "src/Y2024/D17.txt"

-- main :: IO ()
-- main = do
--   parseTestAndSolve parseInput partTwo partTwoTests "src/Y2023/D12.txt"
--   putStrLn "BENCH!"

-- ghcid --command="cabal repl" --test "AoC.parseTestAndSolve $1.$2.parseInput $1.$2.$3 $1.$2.$3Tests \"$4\""
