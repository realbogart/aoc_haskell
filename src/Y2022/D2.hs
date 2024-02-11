module Y2022.D2 where

import AoC

default (Text, Int)

data Move = Rock | Paper | Scissors
  deriving (Show)

data Round = Round
  { opponent :: Move,
    you :: Move
  }
  deriving (Show)

parseInput = parseLineSeparated parseRound
  where
    parseRound = do
      o <- choice [Rock <$ char 'A', Paper <$ char 'B', Scissors <$ char 'C']
      _ <- hspace1
      y <- choice [Rock <$ char 'X', Paper <$ char 'Y', Scissors <$ char 'Z']
      return $ Round o y

input = "A Y\nB X\nC Z"

partOneTests = [(input, 15)]

partTwoTests = [(input, 12)]

scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

scoreOutcome Rock Paper = 6
scoreOutcome Rock Scissors = 0
scoreOutcome Paper Scissors = 6
scoreOutcome Paper Rock = 0
scoreOutcome Scissors Rock = 6
scoreOutcome Scissors Paper = 0
scoreOutcome _ _ = 3

scoreRound :: Move -> Move -> Int
scoreRound o y = scoreMove y + scoreOutcome o y

translateMove :: Move -> Move -> Move
translateMove Rock Rock = Scissors
translateMove Rock Scissors = Paper
translateMove Paper Rock = Rock
translateMove Paper Scissors = Scissors
translateMove Scissors Rock = Paper
translateMove Scissors Scissors = Rock
translateMove o Paper = o

partOne :: [Round] -> Int
partOne = sum . map (uncurry scoreRound . (\r -> (r.opponent, r.you)))

partTwo :: [Round] -> Int
partTwo = sum . map (uncurry scoreRound . (\r -> (r.opponent, translateMove r.opponent r.you)))
