module Main (main) where

import Data.List (sortBy)
import Data.List.Split (splitOn)

type Calories = Int
type Inventory = [Calories]

-- Groups input file lines by inventory.
groupLines :: [String] -> [[String]]
groupLines = splitOn [""]

-- Converts grouped input file lines into inventories.
linesToInventories :: [[String]] -> [Inventory]
linesToInventories = map (map read)

-- Calculates the caloric sum of each inventory given an array of inventories.
sumInventories :: [Inventory] -> [Calories]
sumInventories = map sum

-- Gets the top three calories given an array of calories.
getTopThreeInventories :: [Calories] -> [Calories]
getTopThreeInventories inventories = take 3 $ sortBy (flip compare) inventories

main :: IO()
main = do
    inputFile <- readFile "../input.txt"
    let topThreeInventories = getTopThreeInventories $ sumInventories $ linesToInventories $ groupLines $ lines inputFile
    let result1 = head topThreeInventories
    let result2 = sum topThreeInventories
    print ("Part 1 answer: " ++ show result1)
    print ("Part 2 answer: " ++ show result2)
