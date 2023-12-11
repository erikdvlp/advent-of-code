module Space where

import Data.List (tails)

type Space = [String]
type Coords = (Int, Int)

-- Checks whether a row contains only empty space.
isRowEmpty :: String -> Bool
isRowEmpty s = all (== '.') s

-- Gets all rows that contain only empty space from a given space.
getEmptyRows :: Int -> Space -> [Int]
getEmptyRows _ [] = []
getEmptyRows y (g : gs)
    | isRowEmpty g = y : getEmptyRows (y + 1) gs
    | otherwise = getEmptyRows (y + 1) gs

-- Checks whether a column of a given X value contains only empty space.
isColEmpty :: Int -> Space -> Bool
isColEmpty _ [] = True
isColEmpty x (g : gs)
    | g !! x == '.' = isColEmpty x gs
    | otherwise = False

-- Gets all columns that contain only empty space from a given space.
getEmptyCols :: Int -> Space -> [Int]
getEmptyCols x galaxy@(g : gs)
    | x >= length g = []
    | isColEmpty x galaxy = x : getEmptyCols (x + 1) galaxy
    | otherwise = getEmptyCols (x + 1) galaxy

-- Gets the coordinates of all galaxies in a given space.
getGalaxyCoords :: Coords -> Space -> [Coords]
getGalaxyCoords (x, y) space
    | x >= xMax && y >= yMax = []
    | (space !! y) !! x == '#' = (x, y) : getGalaxyCoords newCoords space
    | otherwise = getGalaxyCoords newCoords space
  where
    xMax = length (head space) - 1
    yMax = length space - 1
    newCoords =
        if x == xMax
            then (0, y + 1)
            else (x + 1, y)

-- Gets unique pairs of each given coordinates.
pairCoords :: [Coords] -> [(Coords, Coords)]
pairCoords coords = [(x, y) | (x : ys) <- tails coords, y <- ys]

-- Calculates the distance between two given coordinates.
calcDistance :: Int -> [Int] -> [Int] -> (Coords, Coords) -> Int
calcDistance n emptyRows emptyCols coords@((x1, y1), (x2, y2)) = rowCrosses + colCrosses + (abs $ (abs $ x1 - x2) + (abs $ y1 - y2))
  where
    isInRange a b c = a >= (min b c) && a <= (max b c)
    doesCrossRow ((_, y1), (_, y2)) y = isInRange y y1 y2
    doesCrossCol ((x1, _), (x2, _)) x = isInRange x x1 x2
    calcCrosses f e = n * (length $ filter (== True) $ map (f coords) e)
    rowCrosses = calcCrosses doesCrossRow emptyRows
    colCrosses = calcCrosses doesCrossCol emptyCols
