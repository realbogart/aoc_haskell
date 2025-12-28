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

isVertical :: (Tile, Tile) -> Bool
isVertical ((_, ay), (_, by)) = ay /= by

left :: (Tile, Tile) -> Int
left ((ax, _), (bx, _)) = min ax bx

right :: (Tile, Tile) -> Int
right ((ax, _), (bx, _)) = max ax bx

top :: (Tile, Tile) -> Int
top ((_, ay), (_, by)) = min ay by

bottom :: (Tile, Tile) -> Int
bottom ((_, ay), (_, by)) = max ay by

intersects :: (Tile, Tile) -> (Tile, Tile) -> Bool
intersects a@((asx, asy), _) b@((bsx, bsy), _)
  | vertical_a = bsy < bottom a && bsy > top a && asx > left b && asx < right b
  | vertical_b = asy < bottom b && asy > top b && bsx > left a && bsx < right a
  | otherwise = False
  where
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
    valid_rectangles = filter (rectangleInside all_lines) rectangles
    sizes = map (uncurry rectangleSize . fst) valid_rectangles
