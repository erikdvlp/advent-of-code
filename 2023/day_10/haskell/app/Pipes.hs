module Pipes where

import Data.Maybe (fromJust, isJust)
import Grid

type PipeType = Char
data Direction = North | South | East | West deriving (Eq)
data Pipe = Pipe {pipeType :: PipeType, coords :: Coords} deriving (Eq)

-- Gets a pipe at given coordinates from a given grid.
getPipeFromGrid :: Grid -> Coords -> Maybe Pipe
getPipeFromGrid grid coords = getFromGrid grid coords >>= \x -> return Pipe{pipeType = x, coords = coords}

-- Gets the valid directions to which a given pipe allows travel.
pipeDirs '|' = [North, South]
pipeDirs '-' = [East, West]
pipeDirs 'L' = [North, East]
pipeDirs 'J' = [North, West]
pipeDirs '7' = [South, West]
pipeDirs 'F' = [South, East]
pipeDirs 'S' = [North, South, East, West]
pipeDirs _ = []

-- Checks whether given pipe types connect with one another from a given direction.
doPipesConnect :: PipeType -> Direction -> PipeType -> Bool
doPipesConnect type1 dir type2 = dir `elem` pipeDirs type1 && dirOpp dir `elem` pipeDirs type2
  where
    dirOpp North = South
    dirOpp South = North
    dirOpp East = West
    dirOpp West = East

-- Gets all valid connections to a given pipe in a given grid.
getValidConnections :: Grid -> Pipe -> [Pipe]
getValidConnections grid (Pipe pipe1Type (x, y)) = map snd $ filter (\(dir, pipe2) -> doPipesConnect pipe1Type dir (pipeType pipe2)) pipes
  where
    north = (North, getPipeFromGrid grid (x, y - 1))
    east = (East, getPipeFromGrid grid (x + 1, y))
    south = (South, getPipeFromGrid grid (x, y + 1))
    west = (West, getPipeFromGrid grid (x - 1, y))
    pipes = filter (\(_, x) -> pipeType x /= '.') $ map (\(x, y) -> (x, fromJust y)) $ filter (\(_, x) -> isJust x) [north, east, south, west]

-- Traverses a given grid and finds the valid path of pipes.
findPath :: Grid -> [Pipe] -> Pipe -> [Pipe]
findPath grid found curr
    | null next = curr : found
    | otherwise = findPath grid (curr : found) $ head next
  where
    next = filter (`notElem` found) (getValidConnections grid curr)

-- Checks whether a given pipe is inside a given path of pipes.
-- Only used for part 2 of the problem.
isInsidePath :: Grid -> [Pipe] -> Maybe Pipe -> Int -> Bool
isInsidePath _ _ Nothing crosses = odd crosses
isInsidePath grid path (Just pipe@(Pipe t (x, y))) crosses
    | t `elem` ['7', 'L'] && pipe `elem` path = next (crosses + 2)
    | pipe `elem` path = next (crosses + 1)
    | otherwise = next crosses
  where
    next = isInsidePath grid path (getPipeFromGrid grid (x + 1, y + 1))

-- Counts how many tiles in a given grid are inside a given path of pipes.
-- Only used for part 2 of the problem.
countInsidePath :: Grid -> [Pipe] -> Coords -> Int
countInsidePath grid path coords@(x, y)
    | not (isValidCoords grid coords) = 0
    | isOnPath = next
    | isInsidePath grid path curr 0 = next + 1
    | otherwise = next
  where
    curr = getPipeFromGrid grid coords
    isOnPath = fromJust curr `elem` path
    newCoords =
        if x == gridWidth grid - 1
            then (0, y + 1)
            else (x + 1, y)
    next = countInsidePath grid path newCoords
