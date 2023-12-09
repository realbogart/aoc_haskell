module Y2018.D3 where

import AoC

default (Text, Int)

data Claim = Claim
  { cid :: Int
  , x :: Int
  , y :: Int
  , w :: Int
  , h :: Int
  } deriving (Show, Eq, Ord)

data Square = Square
  { sx :: Int
  , sy :: Int
  } deriving (Show, Eq, Ord)

parseInput = parseLineSeparated parseClaim
  where parseClaim = do
          _ <- char '#'
          pid <- decimal
          _ <- string " @ "
          px <- decimal
          _ <- char ','
          py <- decimal
          _ <- string ": "
          pw <- decimal
          _ <- char 'x'
          ph <- decimal
          return $ Claim pid px py pw ph

input = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" 

partOneTests = [(input, 4)]
partTwoTests = [(input, 3)]

getClaimSquares :: Claim -> [Square]
getClaimSquares c = [Square sx sy | sx <- [c.x..(c.w + c.x - 1)], sy <- [c.y..(c.h + c.y - 1)]]

claimsIntersect :: Claim -> Claim -> Bool
claimsIntersect a b = a_x + a_w > b_x &&
                      a_y + a_h > b_y &&
                      b_x + b_w > a_x &&
                      b_y + b_h > a_y
  where a_x = a.x
        a_y = a.y
        a_w = a.w
        a_h = a.h
        b_x = b.x
        b_y = b.y
        b_w = b.w
        b_h = b.h

partOne :: [Claim] -> Int
partOne = length . filter ((> 1) . length) . group . sort . concatMap getClaimSquares

partTwo :: [Claim] -> Int
partTwo v = (head $ foldl' deleteClaims v tests).cid
  where tests = filter (uncurry claimsIntersect) . comb2 $ v
        deleteClaims claims (a, b) = delete a . delete b $ claims 

