module Grid where

import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Char)
type Coords = (Int, Int)

-- Parses given input files and converts them into a grid.
linesToGrid :: [String] -> Grid
linesToGrid lines = V.fromList $ map V.fromList lines

-- Gets the height of the grid.
gridHeight :: Grid -> Int
gridHeight = length

-- Gets the width of the grid.
gridWidth :: Grid -> Int
gridWidth grid = length $ V.head grid

-- Checks whether given coordinates are valid within a given grid.
isValidCoords :: Grid -> Coords -> Bool
isValidCoords grid (x, y) = x >= 0 && x < gridWidth grid && y >= 0 && y < gridHeight grid

-- Gets a value at given coordinates from a given grid.
getFromGrid :: Grid -> Coords -> Maybe Char
getFromGrid grid coords@(x, y) =
    if isValidCoords grid coords
        then Just (grid V.! y V.! x)
        else Nothing

-- Gets the coordinates of a target value within a grid.
findInGrid :: Grid -> Char -> Coords -> Coords
findInGrid grid target coords@(x, y)
    | getFromGrid grid coords == Just target = coords
    | otherwise = findInGrid grid target newCoords
  where
    newCoords =
        if x == gridWidth grid - 1
            then (0, y + 1)
            else (x + 1, y)
