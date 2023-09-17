module Y2019.D2 where

import AoC
import Data.Vector qualified as V

default (Text, Int)

parseInput = V.fromList <$> some xs
  where xs :: Parser Int 
        xs = do x <- decimal
                _ <- optional (char ',')
                return x

-- Test is disabled because it behaves differently than than real input (which sets 12 2)
-- partOneTests = [("1,9,10,3,2,3,11,0,99,30,40,50",3500)]
partOneTests = []

partOne :: V.Vector Int -> Int
partOne = init_and_execute 12 2
  where init_and_execute a b v = execute 0 $ V.update v (V.fromList [(1, a), (2, b)])
        execute pc v  | op == 99 = (V.!) v 0
                      | otherwise = execute (pc + 4) execute_op
          where execute_op = V.update v (V.fromList [(target, op_f arg1 arg2)])
                op      = (V.!) v           (pc + 0)
                arg1    = (V.!) v ((V.!) v  (pc + 1))
                arg2    = (V.!) v ((V.!) v  (pc + 2))
                target  = (V.!) v           (pc + 3)
                op_f = case op of
                            1 -> (+)
                            2 -> (*)
                            _ -> error "Invalid op"
