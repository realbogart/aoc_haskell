module Y2023.D2 where

import AoC

default (Int, Text)

data Set = Set
  { red :: Int
  , green :: Int
  , blue :: Int
  } deriving (Show)

data Game = Game
  { id :: Int
  , sets :: [Set]
  } deriving (Show)

defaultSet = Set 0 0 0

red s = s.red
green s = s.green
blue s = s.blue

combineColors :: [Set] -> Set
combineColors = foldl' f defaultSet
  where f :: Set -> Set -> Set
        f a b = Set (a.red + b.red) (a.green + b.green) (a.blue + b.blue)

parseInput = parseLineSeparated parseGame
  where parseGame = do
          _ <- string "Game "
          game_id <- decimal <* char ':'
          sets <- some parseSet
          return $ Game game_id sets
        parseSet = do
          s <- some (hspace *> parseSingleColor <* optional (char ','))
          _ <- optional (char ';' <|> char '\n')
          return $ combineColors s
        parseSingleColor = do
          amount <- decimal <* hspace
          single <- choice [defaultSet{red=amount} <$ string "red", 
                            defaultSet{green=amount} <$ string "green",
                            defaultSet{blue=amount} <$ string "blue"]
          return single

input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

partOneTests = [(input, 8)]
partTwoTests = [(input, 2286)]

testSet = Set 12 13 14

setPossible t c = c.red <= t.red && c.green <= t.green && c.blue <= t.blue
gamePossible g = all (setPossible testSet) g.sets

fewestCubes :: Game -> Set
fewestCubes g = Set (maximum (map red g.sets)) 
                    (maximum (map green g.sets)) 
                    (maximum (map blue g.sets))

setPower s = s.red * s.green * s.blue

partOne :: [Game] -> Int
partOne = sum . map (\g -> g.id) . filter gamePossible

partTwo :: [Game] -> Int
partTwo = sum . map (setPower . fewestCubes)
