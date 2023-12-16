module Springs where

import Data.List (intercalate)
import Data.List.Split (splitOn)

type Spring = Char
type Groups = [Int]
type Row = ([Spring], Groups)

-- Converts a given input file line into a row.
lineToRow :: String -> Row
lineToRow line = (springs, groups)
  where
    parts = words line
    springs = head parts
    groups = map read $ splitOn "," $ last parts

-- Checks if a given row is valid by checking the given springs against the given groups.
isValidRow :: Bool -> Row -> Bool
isValidRow _ ([], groups) = all (== 0) groups
isValidRow _ (springs, []) = all (== '.') springs
isValidRow inGroup (springs@(x : xs), groups@(y : ys))
    | y == 0 && x == '.' = isValidRow False (springs, ys)
    | inGroup && x == '.' = False
    | x == '#' = isValidRow True (xs, y - 1 : ys)
    | otherwise = isValidRow False (xs, groups)

-- Uses backtracking to generate all possible candidates and then checks if rows are valid.
backtrack :: String -> Row -> Int
backtrack trial ([], groups)
    | isValidRow False (reverse trial, groups) = 1
    | otherwise = 0
backtrack trial (x : xs, groups)
    | x == '?' = backtrack ('.' : trial) (xs, groups) + backtrack ('#' : trial) (xs, groups)
    | otherwise = backtrack (x : trial) (xs, groups)

-- Unfolds a given row by repeating its springs and groups five times.
-- Only used for part 2 of the problem.
unfoldRow :: Row -> Row
unfoldRow (springs, groups) = (unfoldedSprings, unfoldedGroups)
  where
    unfoldedSprings = intercalate "?" $ replicate 5 springs
    unfoldedGroups = take (5 * length groups) $ cycle groups
