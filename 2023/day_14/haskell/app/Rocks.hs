module Rocks where

import Data.List (intercalate, sortBy, transpose)
import Data.List.Split (splitOn)
import Prelude hiding (Left, Right)

type Row = String
type Platform = [Row]
data Direction = Left | Right
data CardinalDirection = North | South | East | West

-- Shifts round rocks left or right in a given row.
shiftRow :: Direction -> Row -> Row
shiftRow dir row = intercalate "#" $ map (sortBy $ compareTiles dir) $ splitOn "#" row
  where
    compareTiles Left 'O' '.' = LT
    compareTiles Left '.' 'O' = GT
    compareTiles Right 'O' '.' = GT
    compareTiles Right '.' 'O' = LT
    compareTiles _ _ _ = EQ

-- Spins a given platform north, west, south, and east a given number of times.
spinPlatform :: Platform -> Int -> [Platform]
spinPlatform platform 0 = [platform]
spinPlatform platform cycles = shiftedEast : spinPlatform shiftedEast (cycles - 1)
  where
    shiftedNorth = transpose $ map (shiftRow Left) $ transpose platform
    shiftedWest = map (shiftRow Left) shiftedNorth
    shiftedSouth = transpose $ map (shiftRow Right) $ transpose shiftedWest
    shiftedEast = map (shiftRow Right) shiftedSouth

-- Calculates the load of a given platform.
calcLoad :: Int -> Platform -> Int
calcLoad _ [] = 0
calcLoad weight (x : xs) = rowLoad + calcLoad (weight - 1) xs
  where
    rowLoad = length (filter (== 'O') x) * weight
