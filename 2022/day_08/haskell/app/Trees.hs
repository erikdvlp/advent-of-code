module Trees where

import Data.Char (ord)
import Prelude hiding (Left, Right)

type Forest = [String]
type Coords = (Int, Int)
type Height = Int
data Direction = Left | Right | Top | Bottom

-- Gets the maximum X coordinate possible for a forest.
xMax :: Forest -> Int
xMax f = length (head f) - 1

-- Gets the maximum Y coordinate possible for a forest.
yMax :: Forest -> Int
yMax f = length f - 1

-- Gets the height of a given tree.
treeHeight :: Forest -> Coords -> Height
treeHeight f (x, y) = ord $ f !! y !! x

-- Checks if a given tree in a forest is visible from a given direction.
isTreeVisibleFrom :: Direction -> Height -> Forest -> Coords -> Bool
isTreeVisibleFrom Left h f (x, y)
    | x == 0 = True
    | otherwise = h > treeHeight f (x - 1, y) && isTreeVisibleFrom Left h f (x - 1, y)
isTreeVisibleFrom Right h f (x, y)
    | x == xMax f = True
    | otherwise = h > treeHeight f (x + 1, y) && isTreeVisibleFrom Right h f (x + 1, y)
isTreeVisibleFrom Top h f (x, y)
    | y == 0 = True
    | otherwise = h > treeHeight f (x, y - 1) && isTreeVisibleFrom Top h f (x, y - 1)
isTreeVisibleFrom Bottom h f (x, y)
    | y == yMax f = True
    | otherwise = h > treeHeight f (x, y + 1) && isTreeVisibleFrom Bottom h f (x, y + 1)

-- Checks if a given tree in a forest is visible.
isTreeVisible :: Forest -> Coords -> Bool
isTreeVisible f (x, y) =
    isTreeVisibleFrom Left h f (x, y)
        || isTreeVisibleFrom Right h f (x, y)
        || isTreeVisibleFrom Top h f (x, y)
        || isTreeVisibleFrom Bottom h f (x, y)
  where
    h = treeHeight f (x, y)

-- Traverses every tree in a forest and returns the number of visible trees.
treesVisible :: Forest -> Coords -> Int -> Int
treesVisible f (x, y) count
    | x == xMax f && y == yMax f = count + fromEnum (isTreeVisible f (x, y))
    | otherwise = treesVisible f nextCoords (count + fromEnum (isTreeVisible f (x, y)))
  where
    nextCoords =
        if x == xMax f
            then (0, y + 1)
            else (x + 1, y)
