module Y2025.D9 where

import AoC
import Data.HashMap.Strict qualified as HMS

default (Text, Int)

type Tile = (Int, Int)

data Dir = DirUp | DirDown | DirLeft | DirRight | DirNone | DirUpLeft | DirUpRight | DirDownLeft | DirDownRight deriving (Eq, Show)

type RedGreen = HMS.HashMap Tile Dir

parseInput :: Parser [Tile]
parseInput = parseLineSeparated $ do
  x <- parseInteger <* char ','
  y <- parseInteger
  return (x, y)

input = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"

partOneTests = [(input, 50)]

partTwoTests = [(input, 24)]

rectangleSize :: Tile -> Tile -> Int
rectangleSize (ax, ay) (bx, by) = (abs (ax - bx) + 1) * (abs (ay - by) + 1)

rectTiles :: (Tile, Tile) -> [Tile]
rectTiles ((ax, ay), (bx, by)) = [(x, y) | x <- [(min ax bx) .. (max ax bx)], y <- [(min ay by) .. (max ay by)]]

isVertical :: (Tile, Tile) -> Bool
isVertical ((_, ay), (_, by)) = ay /= by

onLine :: Tile -> (Tile, Tile) -> Bool
onLine (tx, ty) ((lax, lay), (lbx, lby))
  | tx == lax && tx == lbx && ty >= min lay lby && ty <= max lay lby = True
  | ty == lay && ty == lby && tx >= min lax lbx && tx <= max lax lbx = True
  | otherwise = False

getDir :: (Tile, Tile) -> Dir
getDir ((ax, ay), (bx, by))
  | bx > ax = DirRight
  | bx < ax = DirLeft
  | by > ay = DirDown
  | by < ay = DirUp
  | otherwise = error "No dir"

getTurn :: (Tile, Tile) -> (Tile, Tile) -> Dir
getTurn a b
  | dir_a == dir_b = DirNone
  | dir_a == DirRight && dir_b == DirDown = DirRight
  | dir_a == DirRight && dir_b == DirUp = DirLeft
  | dir_a == DirDown && dir_b == DirRight = DirLeft
  | dir_a == DirDown && dir_b == DirLeft = DirRight
  | dir_a == DirLeft && dir_b == DirUp = DirRight
  | dir_a == DirLeft && dir_b == DirDown = DirLeft
  | dir_a == DirUp && dir_b == DirRight = DirRight
  | dir_a == DirUp && dir_b == DirLeft = DirLeft
  | otherwise = error "Invalid turn"
  where
    dir_a = getDir a
    dir_b = getDir b

winds :: [(Tile, Tile)] -> Dir
winds all_lines
  | wind_count > 0 = DirRight
  | otherwise = DirLeft
  where
    wind_count = snd $ foldl' wind (head all_lines, 0) all_lines
    wind ((a, b), acc) l
      | turn == DirRight = (l, acc + 1)
      | turn == DirLeft = (l, acc - 1)
      | otherwise = (l, acc)
      where
        turn = getTurn (a, b) l

normalDir :: Dir -> Dir -> Dir
normalDir fill_dir line_dir
  | line_dir == DirLeft && fill_dir == DirRight = DirUp
  | line_dir == DirLeft && fill_dir == DirLeft = DirDown
  | line_dir == DirRight && fill_dir == DirLeft = DirUp
  | line_dir == DirRight && fill_dir == DirRight = DirDown
  | line_dir == DirDown && fill_dir == DirRight = DirLeft
  | line_dir == DirDown && fill_dir == DirLeft = DirRight
  | line_dir == DirUp && fill_dir == DirRight = DirRight
  | line_dir == DirUp && fill_dir == DirLeft = DirLeft
  | otherwise = error ("Invalid fill dir: " ++ show fill_dir)

cornerNormal :: Dir -> Dir -> Dir
cornerNormal DirUp DirRight = DirUpRight
cornerNormal DirUp DirLeft = DirUpLeft
cornerNormal DirRight DirDown = DirDownRight
cornerNormal DirRight DirUp = DirUpRight
cornerNormal DirDown DirLeft = DirDownLeft
cornerNormal DirDown DirRight = DirDownRight
cornerNormal DirLeft DirUp = DirUpLeft
cornerNormal DirLeft DirDown = DirDownLeft
cornerNormal _ _ = error "Invalid corner"

anyUp :: Dir -> Bool
anyUp DirUp = True
anyUp DirUpLeft = True
anyUp DirUpRight = True
anyUp _ = False

anyDown :: Dir -> Bool
anyDown DirDown = True
anyDown DirDownLeft = True
anyDown DirDownRight = True
anyDown _ = False

anyLeft :: Dir -> Bool
anyLeft DirLeft = True
anyLeft DirUpLeft = True
anyLeft DirDownLeft = True
anyLeft _ = False

anyRight :: Dir -> Bool
anyRight DirRight = True
anyRight DirUpRight = True
anyRight DirDownRight = True
anyRight _ = False

fillLine :: Dir -> RedGreen -> (Tile, Tile) -> RedGreen
fillLine fill_dir tilemap l = foldl' fillTile tilemap line_tiles
  where
    normal_dir = normalDir fill_dir (getDir l)
    line_tiles = rectTiles l
    fillTile tm t = HMS.insert t fill_with tm
      where
        check = HMS.lookup t tm
        fill_with = case check of
          Nothing -> normal_dir
          (Just existing) -> cornerNormal existing normal_dir

floodFill :: RedGreen -> [Tile] -> Tile -> [Tile]
floodFill tilemap visited t@(tx, ty)
  | t `elem` visited = visited
  -- \| otherwise = trace (show tx ++ " : " ++ show ty ++ " : " ++ show normal_dir) $ visited'
  | otherwise = visited'
  where
    visited_with_this = t : visited
    lookup_tile = HMS.lookup t tilemap
    normal_dir = case lookup_tile of
      Nothing -> DirNone
      (Just nd) -> nd
    test_up = if not (anyDown normal_dir) then [(tx, ty - 1)] else []
    test_down = if not (anyUp normal_dir) then [(tx, ty + 1)] else []
    test_left = if not (anyRight normal_dir) then [(tx - 1, ty)] else []
    test_right = if not (anyLeft normal_dir) then [(tx + 1, ty)] else []
    valid_neighbours = concat [test_up, test_down, test_right, test_left]
    visited' = foldl' (floodFill tilemap) visited_with_this valid_neighbours

left :: (Tile, Tile) -> Int
left ((ax, _), (bx, _)) = min ax bx

right :: (Tile, Tile) -> Int
right ((ax, _), (bx, _)) = max ax bx

top :: (Tile, Tile) -> Int
top ((_, ay), (_, by)) = min ay by

bottom :: (Tile, Tile) -> Int
bottom ((_, ay), (_, by)) = max ay by

intersects :: (Tile, Tile) -> (Tile, Tile) -> Bool
intersects a@((asx, asy), _) b@(bs@(bsx, bsy), be)
  -- \| a == b = False
  -- \| a == (be, bs) = False
  -- \| parallel && vertical_a = asx == bsx && ((top a < top b && bottom a > top b) || (top a < bottom b && bottom a > bottom b))
  -- \| parallel && not vertical_a = asy == bsy && ((left a < left b && right a > left b) || (left a < right b && right a > right b))
  | vertical_a = bsy < bottom a && bsy > top a && asx > left b && asx < right b
  | vertical_b = asy < bottom b && asy > top b && bsx > left a && bsx < right a
  | otherwise = False
  where
    -- parallel = vertical_a == vertical_b
    vertical_a = isVertical a
    vertical_b = isVertical b

intersectsRight :: Tile -> (Tile, Tile) -> Bool
intersectsRight (tx, ty) l@((lax, lay), _)
  | isVertical l = tx < lax && ty > top l && ty < bottom l
  | otherwise = ty == lay && tx < left l

pointInside :: [(Tile, Tile)] -> Tile -> Bool
pointInside all_lines t = odd $ length $ filter (intersectsRight t) all_lines

intersectsAny :: [(Tile, Tile)] -> (Tile, Tile) -> Bool
intersectsAny ls l = any (intersects l) ls

rectangleInside :: [(Tile, Tile)] -> ((Tile, Tile), [(Tile, Tile)]) -> Bool
rectangleInside all_lines (rect_pair, rectangle_lines) = corner_check && all_rect_lines_inside && test_point
  where
    -- debug = zip all_lines (map (intersects ((3, 6), (11, 6))) all_lines)
    corners = getRectCorners rect_pair
    corner_check = all (pointInside all_lines) corners
    all_rect_lines_inside = not $ any (intersectsAny all_lines) rectangle_lines
    (bottom_left_x, bottom_left_y) = foldl' (\(acc_x, acc_y) ((ax, ay), _) -> (min acc_x ax, max acc_y ay)) (99999999, 0) rectangle_lines
    test_point = pointInside all_lines (bottom_left_x + 1, bottom_left_y - 1)

getRectLines :: (Tile, Tile) -> [(Tile, Tile)]
getRectLines (a@(ax, ay), b@(bx, by)) = [(a, (bx, ay)), ((bx, ay), b), (b, (ax, by)), ((ax, by), a)]

getRectCorners :: (Tile, Tile) -> [Tile]
getRectCorners (a@(ax, ay), b@(bx, by)) = [a, (bx, ay), b, (ax, by)]

partOne :: [Tile] -> Int
partOne tiles = maximum sizes
  where
    pairs = comb2 tiles
    sizes = map (uncurry rectangleSize) pairs

partTwo :: [Tile] -> Int
partTwo corner_tiles = maximum sizes
  where
    all_lines = zip corner_tiles (tail corner_tiles ++ [head corner_tiles])
    pairs = comb2 corner_tiles
    rectangles = zip pairs (map getRectLines pairs)
    rectangle_tiles = zip pairs (map rectTiles pairs)
    -- valid_rectangles = trace (show rectangle_lines) $ filter (\(_, r) -> rectangleInside all_lines r) rectangle_lines
    valid_rectangles = filter (rectangleInside all_lines) rectangles
    -- valid_rectangles = filter (\(_, r) -> all (pointInside all_lines) r) rectangle_tiles
    sizes = map (uncurry rectangleSize . fst) valid_rectangles
    alternate r = map fst $ filter snd $ zip r (cycle [True, False, False, False, False, False, False, False, False])

-- debug = zip sizes valid_rectangles
