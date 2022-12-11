module Main (main) where

import Data.List (sortBy)
import Data.List.Split (splitOn)

groupCaloriesByElf :: [String] -> [[String]]
groupCaloriesByElf inputLines = splitOn [""] inputLines

readCaloriesAsInt :: [[String]] -> [[Int]]
readCaloriesAsInt = map (map read)

sumCaloriesByElf :: [[Int]] -> [Int]
sumCaloriesByElf caloriesByElf = map sum caloriesByElf

getTop3Elves :: [Int] -> [Int]
getTop3Elves sumCaloriesByElf = take 3 $ sortBy (flip compare) sumCaloriesByElf

main :: IO()
main = do
    inputFile <- readFile "../../inputs/01.txt"
    let top3Elves = getTop3Elves $ sumCaloriesByElf $ readCaloriesAsInt $ groupCaloriesByElf $ lines inputFile
    let result1 = top3Elves !! 0
    let result2 = sum top3Elves
    print result1
    print result2
