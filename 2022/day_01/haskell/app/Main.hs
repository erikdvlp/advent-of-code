module Main (main) where

import Data.List.Split (splitOn)
import Inventory

-- Groups input file lines by inventory.
groupLines :: [String] -> [[String]]
groupLines = splitOn [""]

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let topThreeInventories = getTopThreeInventories $ sumInventories $ linesToInventories $ groupLines $ lines inputFile
    let result1 = head topThreeInventories
    let result2 = sum topThreeInventories
    print ("Part 1 answer: " ++ show result1)
    print ("Part 2 answer: " ++ show result2)
