module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Workflows (calcBounds, calcCombinations, getPartScore, lineToPart, updateWorkflows)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let lineSections = splitOn [""] $ lines inputFile
    let workflows = updateWorkflows Map.empty $ head lineSections
    let parts = map lineToPart $ last lineSections
    let result1 = sum $ map (getPartScore workflows) parts
    print ("Part 1 result: " ++ show result1)
    let bounds = calcBounds workflows "in" ((1, 4000), (1, 4000), (1, 4000), (1, 4000))
    let result2 = calcCombinations bounds
    print ("Part 2 result: " ++ show result2)
