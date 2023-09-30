module Main (main) where

import Items (getCommonItem, getItemPriority, lineToRucksack, linesToElfGroup)

-- Groups input file lines into groups of three for part 2 of the problem.
groupLines :: [String] -> [[String]]
groupLines [] = []
groupLines s = take 3 s : groupLines (drop 3 s)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let run inputLines linesToItemContainer = sum $ map ((getItemPriority . getCommonItem) . linesToItemContainer) inputLines
    let result1 = run (lines inputFile) lineToRucksack
    let result2 = run (groupLines $ lines inputFile) linesToElfGroup
    print ("Part 1 result: " ++ show result1)
    print ("Part 1 result: " ++ show result2)
