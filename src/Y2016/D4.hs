module Y2016.D4 where

import AoC

default (Text, Int)

data Room = Room
  { room_encrypted :: [Char]
  , room_id :: Int
  , room_checksum :: [Char]
  } deriving (Show)

parseInput = parseLineSeparated parseEncrypted
  where parseEncrypted = do
          encryptedName <- some (some letterChar <* char '-')
          rid <- decimal
          _ <- char '['
          chk <- some letterChar
          _ <- char ']'
          return $ Room (concat encryptedName) rid chk

partOneTests = [("aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]", 1514)]
partTwoTests = []

isReal r = commonStr r.room_encrypted == r.room_checksum
  where commonStr = map head . take 5 . sortBy customSort . group . sort
        customSort a b  | la == lb = compare (head a) (head b)
                        | otherwise = compare lb la
          where la = length a
                lb = length b

caesar :: Int -> Char -> Char
caesar n c = chr $ 97 + (((ord c - 97) + n) `mod` 26)

partOne :: [Room] -> Int
partOne = sum . map (\r -> r.room_id) . filter isReal

partTwo :: [Room] -> [String]
partTwo = map applyCaesar . filter isReal
  where applyCaesar r = show r.room_id ++ ": " ++ map (caesar r.room_id) r.room_encrypted 

