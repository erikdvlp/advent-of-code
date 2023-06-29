module Parser where

import Data.List ( isPrefixOf, transpose )
import SupplyStacks ( Move, Stack )

-- Parses input file lines for lines that contain crate data by row.
getCrateLines :: [String] -> [String]
getCrateLines [] = []
getCrateLines (x:xs)
    | " 1" `isPrefixOf` x = []
    | otherwise = x : getCrateLines xs

-- Parses an input file line that contains crate data by row.
crateLineToRow :: String -> String
crateLineToRow [] = []
crateLineToRow line = line !! 1 : crateLineToRow (drop 4 line)

-- Transposes rows of crates into stacks of crates.
rowToStacks :: [String] -> [Stack]
rowToStacks = transpose

-- Removes blank spaces from stacks of crates.
removeSpacesFromStacks :: [Stack] -> [Stack]
removeSpacesFromStacks = map $ filter (/= ' ')

-- Parses input file lines for lines that contain move data.
filterMoveLines :: [String] -> [String]
filterMoveLines = filter $ isPrefixOf "move"

-- Parses an input file line that contains move data into a move.
moveLineToMove :: String -> Move
moveLineToMove s = (head numbers, numbers !! 1, numbers !! 2)
    where
        parts = words s
        numbers = map read [parts !! 1, parts !! 3, parts !! 5]
