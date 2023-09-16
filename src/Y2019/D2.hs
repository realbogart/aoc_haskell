module Y2019.D2 where

import AoC
import Data.Vector qualified as V

default (Text, Int)

parseInput = V.fromList <$> some xs
  where xs :: Parser Int 
        xs = do x <- decimal
                _ <- optional (char ',')
                return x

partOneTests = [("1,9,10,3,2,3,11,0,99,30,40,50",3500)]

partOne :: V.Vector Int -> Int
partOne = execute 0 
  where execute pc v  | op == 99 = (V.!) v 0
                      | otherwise = execute (pc + 4) (V.update v (V.fromList [(target, op_f arg1 arg2)]))
          where op_f = case op of
                            1 -> (+)
                            2 -> (*)
                            _ -> error "Invalid op"
                op = (V.!) v            (pc + 0)
                arg1 = (V.!) v ((V.!) v (pc + 1))
                arg2 = (V.!) v ((V.!) v (pc + 2))
                target = (V.!) v        (pc + 3)
                
