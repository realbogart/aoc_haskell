module Y2024.D17 where

import AoC

default (Integer, Text)

newtype Program = Program [Int]

newtype Output = Output [Int]

instance Show Program where
  show (Program p) = intersperse ',' $ concatMap show p

instance Show Output where
  show (Output o) = intersperse ',' $ concatMap show o

data State = State
  { registerA :: Int,
    registerB :: Int,
    registerC :: Int,
    ip :: Int,
    output :: Output
  }
  deriving (Show)

data Input = Input
  { state :: State,
    program :: Program
  }
  deriving (Show)

parseInput = do
  a <- parseRegister
  b <- parseRegister
  c <- parseRegister
  Input (State a b c 0 (Output [])) <$> parseProgram
  where
    parseRegister :: Parser Int
    parseRegister = string "Register " *> letterChar *> string ": " *> decimal <* space

    parseProgram :: Parser Program
    parseProgram = Program <$> (string "Program: " *> some (decimal <* optional (char ',')))

input = "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"

partOneTests = [(input, "4,6,3,5,6,3,5,2,1,0")]

comboOperand :: State -> Int -> Int
comboOperand s 4 = s.registerA
comboOperand s 5 = s.registerB
comboOperand s 6 = s.registerC
comboOperand _ a
  | a >= 0 && a <= 3 = a
  | otherwise = error "Invalid combo operand"

execute :: State -> (Int, Int) -> State
execute s (0, o) = s {registerA = s.registerA `div` (2 ^ comboOperand s o), ip = s.ip + 2}
execute s (1, o) = s {registerB = xor s.registerB o, ip = s.ip + 2}
execute s (2, o) = s {registerB = mod (comboOperand s o) 8, ip = s.ip + 2}
execute s (3, o)
  | s.registerA == 0 = s {ip = s.ip + 2}
  | otherwise = s {ip = o}
execute s (4, _) = s {registerB = xor s.registerB s.registerC, ip = s.ip + 2}
execute s@State {output = (Output op)} (5, o) = s {output = Output (op ++ [mod (comboOperand s o) 8]), ip = s.ip + 2}
execute s (6, o) = s {registerB = s.registerA `div` (2 ^ comboOperand s o), ip = s.ip + 2}
execute s (7, o) = s {registerC = s.registerA `div` (2 ^ comboOperand s o), ip = s.ip + 2}
execute _ _ = error "Invalid opcode"

pairs (a : b : cs) = (a, b) : pairs cs
pairs [_] = []
pairs [] = []

getInstruction :: Program -> State -> Maybe (Int, Int)
getInstruction (Program p) (State {ip = ip})
  | ip < length p = Just (p !! ip, p !! (ip + 1))
  | otherwise = Nothing

runProgram :: Program -> State -> Output
runProgram p s =
  case instruction of
    Just i -> runProgram p (execute s i)
    -- Just i -> s.output
    Nothing -> s.output
  where
    instruction = getInstruction p s

partOne :: Input -> Text
partOne i = (pack . show . runProgram i.program) i.state
