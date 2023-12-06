module Main where

import Boats (calcWins, linesToRace, linesToRaces)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let result1 = product $ map (calcWins 1) $ linesToRaces $ lines inputFile
    print ("Part 1 result: " ++ show result1)
    let result2 = calcWins 1 $ linesToRace $ lines inputFile
    print ("Part 2 result: " ++ show result2)
