module Y2025.D10 where

import AoC
import Control.Monad (zipWithM_)
import Control.Monad.Loops (untilJust)
import Data.Bits
import Data.HashMap.Strict qualified as HMS
import Data.HashSet qualified as HS
import Data.Heap qualified as H
import Data.SBV qualified as SBV
import Data.STRef
import Data.Vector qualified as V
import Math.LinearEquationSolver
import System.IO.Unsafe (unsafePerformIO)

default (Text, Int)

type Button = Int

type ButtonRaw = [Int]

type ButtonEquation = ([[Integer]], [Integer])

type Indicators = Int

type Joltage = V.Vector Int

type JoltageHash = [Int]

data Machine = Machine
  { indicators_target :: Indicators,
    buttons :: [Button],
    buttons_raw :: [ButtonRaw],
    joltage :: Joltage
  }
  deriving (Show)

data SearchState s = SearchState
  { open :: STRef s (H.Heap (H.Entry Int (Joltage, Int))),
    closed :: STRef s (HS.HashSet JoltageHash),
    score :: STRef s (HMS.HashMap JoltageHash Int)
  }

indicatorBits :: [Char] -> Indicators
indicatorBits cs = foldl' (\acc (i, c) -> if c == '#' then setBit acc i else acc) 0 indices
  where
    indices = zip [0 .. length cs - 1] cs

buttonBits :: ButtonRaw -> Button
buttonBits = foldl' setBit 0

parseMachine :: Parser Machine
parseMachine = do
  indicators <- indicatorBits <$> (char '[' *> (some (char '.' <|> char '#') <* string "] "))
  buttons_raw <- some parseButton
  joltage <- parseJoltage
  return $ Machine indicators (map buttonBits buttons_raw) buttons_raw (V.fromList joltage)
  where
    parseButton :: Parser ButtonRaw
    parseButton = char '(' *> some (parseInteger <* optional (char ',')) <* string ") "

    parseJoltage :: Parser [Int]
    parseJoltage = char '{' *> some (parseInteger <* optional (char ',')) <* char '}'

parseInput :: Parser [Machine]
parseInput = parseLineSeparated parseMachine

input = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

partOneTests =
  [ (input, 7)
  ]

partTwoTests =
  [ (input, 33)
  ]

-- Part 1

pressButtonIndicators :: Indicators -> Button -> Indicators
pressButtonIndicators = xor

pressButtonsIndicators :: [Button] -> Indicators -> [Indicators]
pressButtonsIndicators buttons indicators = map (pressButtonIndicators indicators) buttons

nextIndicators :: [Button] -> [Indicators] -> [Indicators]
nextIndicators buttons = concatMap (pressButtonsIndicators buttons)

findLeastPressesIndicators_ :: [Button] -> Indicators -> [Indicators] -> Int -> Int
findLeastPressesIndicators_ buttons indicators_target all_indicators presses
  | indicators_target `elem` all_indicators_after_presses = presses + 1
  | otherwise = findLeastPressesIndicators_ buttons indicators_target all_indicators_after_presses (presses + 1)
  where
    all_indicators_after_presses = nextIndicators buttons all_indicators

findLeastPressesIndicators :: Machine -> Int
findLeastPressesIndicators (Machine indicators_target buttons _ _) = findLeastPressesIndicators_ buttons indicators_target [0] 0

-- Part 2

pressButtonJoltage :: Joltage -> ButtonRaw -> Joltage
pressButtonJoltage = foldl' increase
  where
    increase j i = V.unsafeUpd j [(i, (j V.! i) + 1)]

pressButtonsJoltage :: [ButtonRaw] -> Joltage -> [Joltage]
pressButtonsJoltage buttons joltage = map (pressButtonJoltage joltage) buttons

validJoltage :: Joltage -> Joltage -> Bool
validJoltage joltage_target joltage = all (\(t, c) -> c <= t) $ V.zip joltage_target joltage

heuristic :: Int -> Joltage -> Joltage -> Int
heuristic max_button target current = div (V.sum $ V.map (uncurry (-)) $ V.zip target current) max_button

findLeastPressesJoltage_ :: SearchState s -> [ButtonRaw] -> Int -> Joltage -> (Joltage, Int) -> ST s (Maybe Int)
findLeastPressesJoltage_ ss buttons max_button target (current_joltage, presses) = do
  let current_joltage_hash = V.toList current_joltage
      neighbours = pressButtonsJoltage buttons current_joltage

  if current_joltage == target
    then trace "Found one!" $ return $ Just presses
    else do
      closed <- readSTRef ss.closed

      if HS.member current_joltage_hash closed
        then return Nothing
        else do
          writeSTRef ss.closed (HS.insert current_joltage_hash closed)
          score <- readSTRef ss.score
          let better_path_exists = case HMS.lookup current_joltage_hash score of
                (Just existing_score) -> existing_score < current_score
                Nothing -> False
          if better_path_exists
            then trace "Found a better path" $ return Nothing
            else do
              modifySTRef
                ss.open
                ( \open ->
                    -- trace ("open: " ++ show open) $ foldl' (\o n -> H.insert (H.Entry (presses + 1 + heuristic max_button target n) (n, presses + 1)) o) open $ filter (validJoltage target) neighbours
                    foldl' (\o n -> H.insert (H.Entry (presses + 1 + heuristic max_button target n) (n, presses + 1)) o) open $ filter (validJoltage target) neighbours
                )
              return Nothing
  where
    current_score = presses + heuristic max_button target current_joltage

findLeastPressesJoltage :: Machine -> Maybe Int
findLeastPressesJoltage (Machine _ _ buttons_raw joltage_target) =
  runST $ do
    ref_open <- newSTRef (H.fromList [H.Entry 0 (V.replicate (V.length joltage_target) 0, 0)])
    ref_closed <- newSTRef HS.empty
    ref_score <- newSTRef HMS.empty

    let state = SearchState ref_open ref_closed ref_score
        max_button = maximum $ map length buttons_raw

    untilJust $ do
      open <- readSTRef ref_open
      case H.viewMin open of
        Nothing -> trace "Found nothing..." $ return $ Just Nothing
        -- Just (H.Entry score next_open, open') -> trace ((show next_open) ++ " : " ++ (show score)) $ do
        Just (H.Entry _ next_open, open') -> do
          writeSTRef ref_open open'
          eval_result <- findLeastPressesJoltage_ state buttons_raw max_button joltage_target next_open
          case eval_result of
            (Just solution) -> return $ Just (Just solution)
            Nothing -> return Nothing

buttonEquation :: Machine -> ButtonEquation
-- buttonEquation (Machine _ _ buttons_raw joltage_target) = trace ((show button_matrix) ++ " : " ++ show answer) $ (button_matrix, answer)
buttonEquation (Machine _ _ buttons_raw joltage_target) = (button_matrix, answer)
  where
    num_joltage = length joltage_target
    joltage_indices = [0 .. num_joltage - 1]
    hasJoltageIndex i = map (\button -> if i `elem` button then 1 else 0) buttons_raw
    button_matrix :: [[Integer]] = map hasJoltageIndex joltage_indices
    answer = map fromIntegral $ V.toList joltage_target

solveEquation :: ButtonEquation -> Int
-- solveEquation (a, b) = trace (show solved_equations) $ fromIntegral $ minimum $ map sum valid_equations
solveEquation (a, b) = fromIntegral $ minimum $ map sum valid_equations
  where
    solved_equations = unsafePerformIO $ solveIntegerLinearEqsAll Z3 2000 a b
    valid_equations = filter (all (>= 0)) solved_equations

solveEquationSBV :: ButtonEquation -> Int
solveEquationSBV (a, b) = unsafePerformIO $ do
  let num_buttons = length (head a)
      vars = ["x" ++ show i | i <- [0 .. num_buttons - 1]]

  result <- SBV.optimize SBV.Lexicographic $ do
    xs <- SBV.sIntegers vars

    mapM_ (\x -> SBV.constrain $ x SBV..>= 0) xs
    SBV.minimize "min_xs" (sum xs)

    zipWithM_
      ( \button_row joltage ->
          SBV.constrain $
            sum
              ( zipWith
                  (\button x -> SBV.literal button * x)
                  button_row
                  xs
              )
              SBV..== SBV.literal joltage
      )
      a
      b

    pure SBV.sTrue

  let result_xs :: [Integer] = case result of
        (SBV.LexicographicResult model) ->
          mapMaybe (`SBV.getModelValue` model) vars
        _ -> trace "no result" []

  -- trace (show result_xs) $ return $ fromInteger $ sum result_xs
  return $ fromInteger $ sum result_xs

testSBV :: Int
testSBV = unsafePerformIO $ do
  result <- SBV.sat $ do
    x <- SBV.sInteger "x"
    y <- SBV.sInteger "y"

    SBV.constrain $ 2 * x + 3 * y SBV..== 10
    SBV.constrain $ y SBV..== 50

    pure SBV.sTrue

  let mx = SBV.getModelValue "x" result

  return $ fromInteger $ fromMaybe 50 mx

partOne :: [Machine] -> Int
partOne = sum . map findLeastPressesIndicators

partTwo :: [Machine] -> Int
partTwo machines = sum $ map (solveEquationSBV . buttonEquation) machines
