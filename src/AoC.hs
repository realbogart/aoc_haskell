module AoC where

import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void

type Parser = Parsec Void Text

parseAndApply :: Show b => Parser a -> (a -> b) -> FilePath -> IO ()
parseAndApply p f inputfile = do
  content <- readFile inputfile
  let parseResult = parse p "" (pack content)
  case parseResult of
    Left err -> putStrLn $ "Failed with error: " ++ errorBundlePretty err
    Right input -> print $ (f input)

