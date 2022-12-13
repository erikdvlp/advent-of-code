module Main (main) where

import Helpers

stringToRucksack :: String -> ItemContainer
stringToRucksack s = Rucksack $ splitAt ((length s + 1) `div` 2) s

stringsToElfGroup :: [String] -> ItemContainer
stringsToElfGroup s = ElfGroup $ (s !! 0, s !! 1, s !! 2)

groupStrings :: [String] -> [[String]]
groupStrings [] = []
groupStrings s = (take 3 s) : (groupStrings (drop 3 s))

main :: IO()
main = do
    inputFile <- readFile "../../inputs/03.txt"
    let run = (\input stringToItemContainer -> sum $ map calcPriority $ map getCommonItem $ map stringToItemContainer $ input)
    let result1 = run (lines inputFile) stringToRucksack
    let result2 = run (groupStrings $ lines inputFile) stringsToElfGroup
    print result1
    print result2
