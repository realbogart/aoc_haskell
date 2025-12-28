module Y2025.D10 where

import AoC
import Data.Bits
import Data.HashMap.Strict qualified as HMS
import Data.HashSet qualified as HS
import Data.Heap qualified as H
import Data.STRef
import Data.Vector qualified as V

default (Text, Int)

type Button = Int

type ButtonRaw = [Int]

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
  { open :: STRef s (H.Heap (H.Entry Int Joltage)),
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

heuristic :: Joltage -> Joltage -> Int
heuristic target current = V.sum $ V.map (uncurry (-)) $ V.zip target current

findLeastPressesJoltage_ :: SearchState s -> [ButtonRaw] -> Joltage -> Joltage -> Int -> ST s (Maybe Int)
findLeastPressesJoltage_ ss buttons target current_joltage presses
  | current_joltage == target = return $ Just (presses + 1)
  | otherwise = do
      let current_joltage_hash = V.toList current_joltage
          neighbours = pressButtonsJoltage buttons current_joltage

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
            then return Nothing
            else do
              open <- readSTRef ss.open
              let open_add_neighbours = foldl' (\o n -> H.insert (H.Entry (presses + 1 + heuristic target n) n) o) open $ filter (validJoltage target) neighbours
                  next_open = H.viewMin open_add_neighbours

              case next_open of
                Nothing -> return Nothing
                Just (H.Entry _ next_joltage, open') -> do
                  writeSTRef ss.open open'
                  findLeastPressesJoltage_ ss buttons target next_joltage (presses + 1)
  where
    current_score = presses + heuristic target current_joltage

findLeastPressesJoltage :: Machine -> Maybe Int
findLeastPressesJoltage (Machine _ _ buttons_raw joltage_target) =
  runST $ do
    open <- newSTRef H.empty
    closed <- newSTRef HS.empty
    score <- newSTRef HMS.empty
    let initial_state = SearchState open closed score
    findLeastPressesJoltage_ initial_state buttons_raw joltage_target (V.replicate (V.length joltage_target) 0) 0

partOne :: [Machine] -> Int
partOne = sum . map findLeastPressesIndicators

partTwo :: [Machine] -> Int
partTwo = sum . mapMaybe findLeastPressesJoltage
