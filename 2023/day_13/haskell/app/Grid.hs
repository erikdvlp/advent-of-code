module Grid where

import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Char)

-- Parses given input files and converts them into a grid.
linesToGrid :: [String] -> Grid
linesToGrid lines = V.fromList $ map V.fromList lines

-- Gets the height of the grid.
gridHeight :: Grid -> Int
gridHeight = length

-- Gets the width of the grid.
gridWidth :: Grid -> Int
gridWidth grid = length $ V.head grid

-- Gets a row at a given Y value from a given grid.
getRowFromGrid :: Grid -> Int -> String
getRowFromGrid grid y = V.toList $ grid V.! y

-- Gets a column at a given X value from a given grid.
getColFromGrid :: Int -> Grid -> Int -> String
getColFromGrid y grid x
    | y < gridHeight grid = grid V.! y V.! x : getColFromGrid (y + 1) grid x
    | otherwise = []
