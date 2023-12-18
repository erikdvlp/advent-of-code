module Beam where

import Grid
import Prelude hiding (Left, Right, traverse)

data Direction = Up | Down | Left | Right deriving (Eq)
type Cache = [(Coords, Direction)]

-- Shifts given coordinates in a given direction.
shiftCoords :: Coords -> Direction -> Coords
shiftCoords (x, y) Up = (x, y - 1)
shiftCoords (x, y) Down = (x, y + 1)
shiftCoords (x, y) Left = (x - 1, y)
shiftCoords (x, y) Right = (x + 1, y)

-- Traverses a beam through a given grid and caches its path.
traverse :: Grid -> Cache -> Coords -> Direction -> Cache
traverse grid cache coords@(x, y) dir
    | not $ isValidCoords grid coords = cache
    | (coords, dir) `elem` cache = cache
    | tile == '|' && dir `elem` [Left, Right] =
        let
            path1 = traverse grid newCache (shiftCoords coords Up) Up
            path2 = traverse grid path1 (shiftCoords coords Down) Down
         in
            path2
    | tile == '-' && dir `elem` [Up, Down] =
        let
            path1 = traverse grid newCache (shiftCoords coords Left) Left
            path2 = traverse grid path1 (shiftCoords coords Right) Right
         in
            path2
    | tile == '/' && dir == Up = traverse grid newCache (shiftCoords coords Right) Right
    | tile == '/' && dir == Down = traverse grid newCache (shiftCoords coords Left) Left
    | tile == '/' && dir == Left = traverse grid newCache (shiftCoords coords Down) Down
    | tile == '/' && dir == Right = traverse grid newCache (shiftCoords coords Up) Up
    | tile == '\\' && dir == Up = traverse grid newCache (shiftCoords coords Left) Left
    | tile == '\\' && dir == Down = traverse grid newCache (shiftCoords coords Right) Right
    | tile == '\\' && dir == Left = traverse grid newCache (shiftCoords coords Up) Up
    | tile == '\\' && dir == Right = traverse grid newCache (shiftCoords coords Down) Down
    | otherwise = traverse grid newCache (shiftCoords coords dir) dir
  where
    tile = getFromGrid grid coords
    newCache = (coords, dir) : cache

-- Calculates all possible starting coordinates in a given grid.
-- Only used in part 2 of the problem.
calcStarts :: Grid -> [(Coords, Direction)]
calcStarts grid = concat [top, bottom, left, right]
  where
    xMax = gridWidth grid - 1
    yMax = gridHeight grid - 1
    top = zip (zip [0 .. xMax] (repeat 0)) (repeat Down)
    bottom = zip (zip [0 .. xMax] (repeat yMax)) (repeat Up)
    left = zip (zip (repeat 0) [0 .. yMax]) (repeat Right)
    right = zip (zip (repeat xMax) [0 .. yMax]) (repeat Left)
