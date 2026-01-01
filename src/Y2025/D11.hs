module Y2025.D11 where

import AoC
import Data.HashMap.Strict qualified as HMS

default (Int, Text)

type Device = Text

type DeviceConnection = (Device, [Device])

type DeviceMap = HMS.HashMap Device [Device]

buildDeviceMap :: [DeviceConnection] -> DeviceMap
buildDeviceMap = foldl' (\dm (from, to) -> HMS.insert from to dm) HMS.empty

parseDeviceConnection :: Parser DeviceConnection
parseDeviceConnection = do
  from <- pack <$> count 3 letterChar <* string ": "
  to <- some (pack <$> count 3 letterChar <* hspace)
  return (from, to)

parseInput :: Parser DeviceMap
parseInput = buildDeviceMap <$> parseLineSeparated parseDeviceConnection

partOneTests =
  [ ("aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out", 5)
  ]

countPaths :: Device -> DeviceMap -> Int
countPaths from dm
  | from == "out" = 1
  | otherwise = sum $ map (`countPaths` dm) connections
  where
    connections = dm HMS.! from

partOne :: DeviceMap -> Int
partOne = countPaths "you"
