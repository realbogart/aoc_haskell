module Y2015.D4 where

import AoC
import Crypto.Hash
import Data.ByteString.Char8 qualified as B

default (Text, Int)

partOneTests = [("abcdef", 609043), ("pqrstuv", 1048970)]
partTwoTests = []

parseInput :: Parser B.ByteString
parseInput = B.pack <$> some letterChar

isCoin ('0':'0':'0':'0':'0':_) = True
isCoin _ = False

isCoin2 ('0':'0':'0':'0':'0':'0':_) = True
isCoin2 _ = False

prepareSecret secret x = secret <> (B.pack $ show x) 

findCoin f secret x | f hashedSecret = x
                    | otherwise = findCoin f secret (x + 1)
  where preparedSecret = prepareSecret secret x
        hashedSecret = show (hash preparedSecret :: Digest MD5)

partOne secret = findCoin isCoin secret 0 
partTwo secret = findCoin isCoin2 secret 0 

