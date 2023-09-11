module AoC (
  Parser,
  module Text.Megaparsec, 
  module Text.Megaparsec.Char,
  module Text.Megaparsec.Char.Lexer,
  module Text.Pretty.Simple,
  module Control.Monad,
  module Control.Applicative,
  module Data.List,
  module Data.Function,
  module Data.Maybe,
  module Data.Char
) where

import Text.Megaparsec(Parsec, parse, manyTill, anySingle, errorBundlePretty, many, eof, choice)
import Text.Megaparsec.Char(eol, letterChar, digitChar, string, char)
import Text.Megaparsec.Char.Lexer(decimal)

import Data.List
import Data.Text (Text, pack)
import Data.Void
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Char (digitToInt)

import Control.Monad (void)
import Control.Applicative ((<|>))

import Text.Pretty.Simple (pPrint)
import qualified Distribution.PackageDescription as Text.Pretty

type Parser = Parsec Void Text

parseAndApply :: Show b => Parser a -> (a -> b) -> FilePath -> IO ()
parseAndApply p f inputfile = do
  content <- readFile inputfile
  let parseResult = parse p "" (pack content)
  case parseResult of
    Left err -> putStrLn $ "Failed with error: " ++ errorBundlePretty err
    Right input -> pPrint $ f input

