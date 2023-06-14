module Main (main) where

import Game ( getRoundPoints, lineToRound )

main :: IO()
main = do
    inputFile <- readFile "../input.txt"
    let run problemPart = sum $ map (getRoundPoints . lineToRound problemPart) (lines inputFile)
    let result1 = run 1
    let result2 = run 2
    print ("Part 1 result: " ++ show result1)
    print ("Part 1 result: " ++ show result2)
