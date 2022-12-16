module Main (main) where

import Helpers
import Data.List (isPrefixOf, transpose)

filterStackLines :: [String] -> [String]
filterStackLines [] = []
filterStackLines s = if isPrefixOf " 1" (head s)
    then []
    else head s : filterStackLines (tail s)

stackLineToRow :: String -> String
stackLineToRow [] = []
stackLineToRow s = s !! 1 : stackLineToRow (drop 4 s)

rowToStacks :: [String] -> [Stack]
rowToStacks s = transpose s

removeSpacesFromStacks :: [Stack] -> [Stack]
removeSpacesFromStacks s = map removeSpaces s
    where
        removeSpaces = filter (\c -> c /= ' ')

filterMoveLines :: [String] -> [String]
filterMoveLines s = filter (isPrefixOf "move") s

moveLineToMove :: String -> Move
moveLineToMove s = (numbers !! 0, numbers !! 1, numbers !! 2)
    where
        parts = words s
        numbers = map read $ [parts !! 1, parts !! 3, parts !! 5]

main :: IO()
main = do
    inputFile <- readFile "../../inputs/05.txt"
    let stacks = removeSpacesFromStacks $ rowToStacks $ map stackLineToRow $ filterStackLines $ lines inputFile
    let moves = map moveLineToMove $ filterMoveLines $ lines inputFile
    let run = (\mover -> getTopsOfStacks $ executeMoves stacks moves mover)
    let result1 = run CM9000
    let result2 = run CM9001
    print result1
    print result2
