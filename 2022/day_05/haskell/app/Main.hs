module Main (main) where

import Parser
import SupplyStacks

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let stacks = removeSpacesFromStacks $ rowToStacks $ map crateLineToRow $ getCrateLines $ lines inputFile
    let moves = map moveLineToMove $ filterMoveLines $ lines inputFile
    let run mover = map head $ executeMoves stacks moves mover
    let result1 = run CM9000
    let result2 = run CM9001
    print ("Part 1 result: " ++ show result1)
    print ("Part 2 result: " ++ show result2)
