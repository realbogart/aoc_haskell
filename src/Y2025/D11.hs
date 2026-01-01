module Y2025.D11 where

import AoC
import Data.HashMap.Strict qualified as HMS
import Data.STRef

default (Int, Text)

type Device = Text

type DeviceConnection = (Device, [Device])

type DeviceMap = HMS.HashMap Device [Device]

type PathMap = HMS.HashMap Device Int

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

partTwoTests =
  [ ("svr: aaa bbb\naaa: fft\nfft: ccc\nbbb: tty\ntty: ccc\nccc: ddd eee\nddd: hub\nhub: fff\neee: dac\ndac: fff\nfff: ggg hhh\nggg: out\nhhh: out", 2)
  ]

countPaths :: Device -> DeviceMap -> Int
countPaths from dm
  | from == "out" = 1
  | otherwise = sum $ map (`countPaths` dm) connections
  where
    connections = dm HMS.! from

countPaths2 :: STRef s PathMap -> Device -> Device -> Bool -> Bool -> Bool -> Int -> DeviceMap -> ST s Int
countPaths2 ref_pm from to has_dac has_fft ignore_dac_fft depth dm
  | from == to && ((found_dac && found_fft) || ignore_dac_fft) = return 1
  | otherwise = do
      pm <- readSTRef ref_pm
      let already_checked = HMS.lookup from pm
      case already_checked of
        (Just existing_num_paths) -> return existing_num_paths
        Nothing ->
          sum
            <$> mapM
              ( \c -> do
                  c_num <- countPaths2 ref_pm c to found_dac found_fft ignore_dac_fft (depth + 1) dm
                  modifySTRef' ref_pm (HMS.insert c c_num)
                  return c_num
              )
              connections
  where
    connections = dm HMS.! from
    found_dac = has_dac || from == "dac"
    found_fft = has_fft || from == "fft"

appendOut :: DeviceMap -> DeviceMap
appendOut = HMS.insert "out" []

invertDeviceMap :: DeviceMap -> DeviceMap
invertDeviceMap dm = foldl' addDeviceConnection HMS.empty (HMS.toList dm)
  where
    getAllTo :: Device -> [Device]
    getAllTo target = map fst $ HMS.toList $ HMS.filter (\to -> target `elem` to) dm

    addDeviceConnection :: DeviceMap -> DeviceConnection -> DeviceMap
    addDeviceConnection new_dm (from, _) = HMS.insert from (getAllTo from) new_dm

countWrap :: DeviceMap -> Device -> Device -> Bool -> ST s Int
countWrap dm from to ignore_dac_fft = do
  pm <- newSTRef HMS.empty
  countPaths2 pm from to False False ignore_dac_fft 0 dm

partOne :: DeviceMap -> Int
partOne = countPaths "you"

partTwo :: DeviceMap -> Int
partTwo dm = runST $ do
  let dm_forward = appendOut dm
  svr_to_fft <- countWrap dm_forward "svr" "fft" True
  fft_to_dac <- countWrap dm_forward "fft" "dac" True
  dac_to_out <- countWrap dm_forward "dac" "out" True
  return $ svr_to_fft * fft_to_dac * dac_to_out
