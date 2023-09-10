module AoC (
  Parser,
  module Text.Megaparsec, 
  module Text.Megaparsec.Char,
  module Text.Megaparsec.Char.Lexer,
  module Data.List,
  module Control.Monad,
  module Control.Applicative,
  module Data.Function
) where

import Text.Megaparsec(Parsec, parse, manyTill, anySingle, errorBundlePretty, many, eof, choice)
import Text.Megaparsec.Char(eol, letterChar, string, char)
import Text.Megaparsec.Char.Lexer(decimal)

import Data.List
import Data.Text (Text, pack)
import Data.Void
import Data.Function (on)

import Control.Monad (void)
import Control.Applicative ((<|>))


type Parser = Parsec Void Text

parseAndApply :: Show b => Parser a -> (a -> b) -> FilePath -> IO ()
parseAndApply p f inputfile = do
  content <- readFile inputfile
  let parseResult = parse p "" (pack content)
  case parseResult of
    Left err -> putStrLn $ "Failed with error: " ++ errorBundlePretty err
    Right input -> print $ f input

