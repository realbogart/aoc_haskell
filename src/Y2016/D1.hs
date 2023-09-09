{-# LANGUAGE OverloadedStrings #-}

module Y2016.D1 where

import AoC
import Control.Monad
import Control.Applicative ((<|>))

data Turn = TurnLeft | TurnRight deriving (Show)
data Move = Move 
  { turn    :: Turn
  , steps   :: Int
  } deriving (Show)

parseMove :: Parser Move
parseMove = do
  turn <- TurnLeft <$ char 'L' <|> TurnRight <$ char 'R'
  steps <- decimal
  delim <- choice [void $ string ", ", eof, void eol]
  return $ Move turn steps

parseInput :: Parser [Move]
parseInput = do
  many parseMove

partOne :: [Move] -> [Move]
partOne = id

