module Main (main) where

import Helpers
import Data.List.Split (splitOn)

stringToAssignmentPair :: String -> AssignmentPair
stringToAssignmentPair s = ((a1 !! 0, a1 !! 1), (a2 !! 0, a2 !! 1))
    where
        stringByElf = splitOn [','] s
        stringByElfToAssignment = (\i -> map read $ splitOn ['-'] (stringByElf !! i))
        a1 = stringByElfToAssignment 0 -- [2, 4]
        a2 = stringByElfToAssignment 1 -- [6, 8]

main :: IO()
main = do
    inputFile <- readFile "../../inputs/04.txt"
    let overlaps = map getOverlap $ map stringToAssignmentPair $ lines inputFile
    let countOverlaps = (\f -> length $ filter f overlaps)
    let result1 = countOverlaps (\overlap -> overlap == Full)
    let result2 = countOverlaps (\overlap -> overlap == Full || overlap == Partial)
    print result1
    print result2
