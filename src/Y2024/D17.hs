{-# LANGUAGE LinearTypes #-}

module Y2024.D17 where

import AoC
-- import Data.Function ((&))

import Control.Monad.Trans.State.Strict qualified as SMT
import Data.Bits (Bits (shiftR))
import Data.Vector.Unboxed qualified as UV

-- import Data.Vector.Unboxed.Mutable qualified as MUV

-- import Data.Vector.Unboxed.Mutable qualified as MUV
-- import Streamly.Data.Fold qualified as Fold
-- import Streamly.Data.Stream qualified as Stream

default (Int, Text)

type Program = UV.Vector Int

type Output = UV.Vector Int

data State = State
  { registerA :: Int,
    registerB :: Int,
    registerC :: Int,
    ip :: Int,
    output :: Output,
    outputCount :: Int
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
  p <- parseProgram
  return $ Input (State a b c 0 (UV.replicate (UV.length p) 0) 0) p
  where
    parseRegister :: Parser Int
    parseRegister = string "Register " *> letterChar *> string ": " *> decimal <* space

    parseProgram :: Parser Program
    parseProgram = UV.fromList <$> (string "Program: " *> some (decimal <* optional (char ',')))

input = "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"

input2 = "Register A: 2024\nRegister B: 0\nRegister C: 0\n\nProgram: 0,3,5,4,3,0"

partOneTests = [(input, "4,6,3,5,6,3,5,2,1,0")]

-- partTwoTests = [(input2, 117440)]

partTwoTests = [] :: [(Text, Int)]

comboOperand :: State -> Int -> Int
comboOperand s 4 = s.registerA
comboOperand s 5 = s.registerB
comboOperand s 6 = s.registerC
comboOperand _ 7 = error "Invalid combo operand"
comboOperand _ a = a

step :: State -> State
step s = s {ip = s.ip + 2}

execute :: State -> (Int, Int) -> State
execute s (0, o) = (step s) {registerA = s.registerA `shiftR` comboOperand s o}
execute s (1, o) = (step s) {registerB = xor s.registerB o}
execute s (2, o) = (step s) {registerB = mod (comboOperand s o) 8}
execute s (3, o)
  | s.registerA == 0 = step s
  | otherwise = s {ip = o}
execute s (4, _) = (step s) {registerB = xor s.registerB s.registerC}
execute s@State {output = op, outputCount = oc} (5, o) = (step s) {output = newOutput, outputCount = oc + 1}
  where
    newOutput = op UV.// [(oc, mod (comboOperand s o) 8)]
execute s (6, o) = (step s) {registerB = s.registerA `shiftR` comboOperand s o}
execute s (7, o) = (step s) {registerC = s.registerA `shiftR` comboOperand s o}
execute _ _ = error "Invalid opcode"

getInstruction :: UV.Vector Int -> State -> Maybe (Int, Int)
getInstruction p (State {ip = ip})
  | ip < UV.length p = Just (p UV.! ip, p UV.! (ip + 1))
  | otherwise = Nothing

runProgram :: UV.Vector Int -> State -> Output
runProgram p s =
  case instruction of
    Just i -> runProgram p $ execute s i
    Nothing -> s.output
  where
    instruction = getInstruction p s

runProgram2 :: UV.Vector Int -> SMT.StateT State Identity State
runProgram2 p = do
  state <- SMT.get
  let instruction = getInstruction p state
  if state.outputCount > 0 && (p UV.! (state.outputCount - 1)) /= (state.output UV.! (state.outputCount - 1))
    then return state
    else case instruction of
      Just i -> do
        SMT.put (execute state i)
        runProgram2 p
      Nothing -> return state

-- runProgramStream :: UV.Vector Int -> State -> Output
-- runProgramStream pv s =
--   runIdentity $
--     Stream.unfoldr f s
--       & Stream.fold Fold.latest
--       & fmap ((\State {output = output} -> output) . fromMaybe s)
--   where
--     f cs =
--       let mi = getInstruction pv cs
--        in case mi of
--             Nothing -> Nothing
--             Just i -> Just (cs, execute cs i)

-- findAStream :: Program -> UV.Vector Int -> State -> Int
-- findAStream p pv s =
--   runIdentity $
--     Stream.iterate (+ 1) s.registerA
--       & fmap debug
--       & Stream.filter (\a -> reverse (runProgramStream pv s {registerA = a}) == p)
--       & Stream.fold Fold.one
--       & fmap (fromMaybe 0)
--   where
--     debug a
--       | mod a 1000000 == 0 = trace (show a) a
--       | otherwise = a

findA :: Program -> State -> Int
findA p s@State {registerA = a}
  | outputCount == UV.length p && output == p = a
  | mod a 1000000 == 0 = trace (show a) $ findA p s {registerA = a + 1}
  | otherwise = findA p s {registerA = a + 1}
  where
    (State {output = output, outputCount = outputCount}, _) = runIdentity $ SMT.runStateT (runProgram2 p) s

partOne :: Input -> Text
partOne i = (pack . show . runProgram i.program) i.state

partTwo :: Input -> Int
partTwo i = findA i.program i.state {registerA = 504792000000}

-- 504792000000
