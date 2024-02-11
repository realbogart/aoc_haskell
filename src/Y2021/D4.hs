module Y2021.D4 where

import AoC
import Data.Vector qualified as V

default (Int, Text)

data Bingo = Bingo
  { numbers :: [Int],
    boards :: [Grid Int]
  }
  deriving (Show)

parseBoard :: Parser (Grid Int)
parseBoard = do
  numbers <- count 25 (parseInteger <* optional space)
  return $ Grid (V.fromList numbers) 5 5

parseInput :: Parser Bingo
parseInput = do
  ns <- some (parseInteger <* optional (char ','))
  _ <- string "\n\n"
  boards <- some (parseBoard <* hspace)
  return $ Bingo ns boards

input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"

partOneTests = [(input, 4512)]

partTwoTests = [(input, 1924)]

lanes :: [[(Int, Int)]]
lanes = rows ++ columns
  where
    rows = [[(x, y) | x <- [0 .. 4]] | y <- [0 .. 4]]
    columns = [[(x, y) | y <- [0 .. 4]] | x <- [0 .. 4]]

markBoard :: Int -> Grid Int -> Grid Int
markBoard target board = Grid (V.fromList $ map (\x -> if x == target then -1 else x) $ V.toList board.grid) 5 5

markBoards :: Int -> [Grid Int] -> [Grid Int]
markBoards target = map (markBoard target)

hasWon :: Grid Int -> Bool
hasWon board = any isLaneFilled lanes
  where
    isLaneFilled = all ((== -1) . getGridValue board)

countScore :: Int -> Grid Int -> Int
countScore n board = n * sum (filter (/= -1) $ V.toList board.grid)

play :: Bingo -> Int
play (Bingo [] _) = error "No winner, ran out of numbers"
play (Bingo (n : ns) boards)
  | not (null winner_boards) = head winner_scores
  | otherwise = play (Bingo ns next_boards)
  where
    next_boards = markBoards n boards
    winner_boards = filter hasWon next_boards
    winner_scores = map (countScore n) winner_boards

playSquid :: Bingo -> Int -> Int
playSquid (Bingo [] _) last_win_score = last_win_score
playSquid (Bingo _ []) last_win_score = last_win_score
playSquid (Bingo (n : ns) boards) _ = playSquid (Bingo ns loser_boards) (countScore n $ head winner_boards)
  where
    next_boards = markBoards n boards
    (winner_boards, loser_boards) = partition hasWon next_boards

partOne :: Bingo -> Int
partOne = play

partTwo :: Bingo -> Int
partTwo boards = playSquid boards 0
