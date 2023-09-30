module Main (main) where

import Assignment (Overlap (Full, Partial), getOverlap, lineToAssignmentPair)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let overlaps = map (getOverlap . lineToAssignmentPair) (lines inputFile)
    let countOverlaps f = length $ filter f overlaps
    let result1 = countOverlaps (== Full)
    let result2 = countOverlaps (\overlap -> overlap == Full || overlap == Partial)
    print ("Part 1 result: " ++ show result1)
    print ("Part 2 result: " ++ show result2)
