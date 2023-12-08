module Main where

import Network (ProblemPart (Part1, Part2), calcStepsToDest, getNodesEndingInA, lineToInstructions, linesToNodeMap)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let instructions = cycle $ lineToInstructions $ head $ lines inputFile
    let nodeMap = linesToNodeMap $ lines inputFile
    let result1 = calcStepsToDest Part1 nodeMap instructions "AAA"
    print ("Part 1 result: " ++ show result1)
    let result2 = foldr (lcm . calcStepsToDest Part2 nodeMap instructions) 1 (getNodesEndingInA nodeMap)
    print ("Part 2 result: " ++ show result2)
