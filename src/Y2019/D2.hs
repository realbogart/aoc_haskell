module Y2019.D2 where

import AoC
import Data.Vector qualified as V

default (Text, Int)

parseInput = V.fromList <$> some xs
  where
    xs :: Parser Int
    xs = do
      x <- decimal
      _ <- optional (char ',')
      return x

-- Tests are disabled because they behave differently to real input (which sets 12 2)
-- partOneTests = [("1,9,10,3,2,3,11,0,99,30,40,50",3500)]
partOneTests = []

partTwoTests = []

execute pc v
  | op == 99 = (V.!) v 0
  | otherwise = execute (pc + 4) execute_op
  where
    execute_op = V.update v (V.fromList [(target, op_f arg1 arg2)])
    op = (V.!) v (pc + 0)
    arg1 = (V.!) v ((V.!) v (pc + 1))
    arg2 = (V.!) v ((V.!) v (pc + 2))
    target = (V.!) v (pc + 3)
    op_f = case op of
      1 -> (+)
      2 -> (*)
      _ -> error "Invalid op"

initAndExecute v a b = execute 0 $ V.update v (V.fromList [(1, a), (2, b)])

partOne v = initAndExecute v 12 2

partTwo = result . findSolution
  where
    findSolution v = find ((== 19690720) . uncurry (initAndExecute v)) [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]
    result (Just (noun, verb)) = noun * 100 + verb
    result Nothing = error "No solution found"
