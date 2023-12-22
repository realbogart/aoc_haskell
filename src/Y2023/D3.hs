module Y2023.D3 where

import AoC

default (Text, Int)

data Part = Part
  { pos :: Int
  , value :: [Char]
  , is_symbol :: Bool
  , is_newline :: Bool
  } deriving (Show)

pos p = p.pos
value p = p.value
isSymbol p = p.is_symbol && not p.is_newline
isValue p = not p.is_symbol && not p.is_newline

parseInput = many (empty_space *> parsePart <* empty_space)
  where empty_space = many (char '.')
        parsePart :: Parser Part
        parsePart = do
          p <- getOffset
          v <- some digitChar <|> singleton <$> latin1Char
          return $ Part p v (not (all isDigit v)) (v == "\n")

input = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

partOneTests = [(input, 4361)]
partTwoTests = [(input, 467835)]

getWidth :: [Part] -> Int
getWidth = pos . head . filter (\p -> p.is_newline)

getPerimiter :: Int -> Part -> [Int]
getPerimiter mapWidth p = filter (>= 0) perim
  where perim = [p.pos - 1, p.pos + w] ++
                [p.pos - 1 - mapWidth .. p.pos + w - mapWidth] ++ 
                [p.pos - 1 + mapWidth .. p.pos + w + mapWidth] 
        w = length p.value

isAdjacent mapWidth symbol part = symbol.pos `elem` perim
  where perim = getPerimiter mapWidth part

hasSymbolAdjacent :: Int -> [Part] -> Part -> Bool
hasSymbolAdjacent mapWidth ss p = any (\s -> s.pos `elem` perim) ss
  where perim = getPerimiter mapWidth p

getAdjacentParts :: Int -> [Part] -> Part -> [Part]
getAdjacentParts mapWidth parts symbol = filter (isAdjacent mapWidth symbol) parts

getAdjacentValues :: Int -> [Part] -> Part -> [Int]
getAdjacentValues mapWidth parts symbol = map (read . value) (getAdjacentParts mapWidth parts symbol)

partOne :: [Part] -> Int
partOne l = sum $ map (read . value) $ filter (hasSymbolAdjacent w symbols) parts
  where w = getWidth l + 1
        symbols = filter isSymbol l
        parts = filter isValue l

partTwo :: [Part] -> Int
partTwo l = sum $ map (product . getAdjacentValues w parts) gears
  where w = getWidth l + 1
        parts = filter isValue l
        potential_gears = filter (\p -> isSymbol p && p.value == "*") l
        gears = filter ((== 2) . length . getAdjacentParts w parts) potential_gears

