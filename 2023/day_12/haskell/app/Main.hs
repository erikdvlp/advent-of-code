module Main where

import Springs

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let rows = map lineToRow $ lines inputFile
    let result1 = sum $ map (backtrack []) rows
    print ("Part 1 result: " ++ show result1)
    let result2 = sum $ map (backtrack []) $ map unfoldRow rows
    print ("Part 2 result: " ++ show result2)
