module Main (main) where

import Helpers

stringToRoundPart1 :: String -> Round
stringToRoundPart1 s = RoundPart1 ((charToShape $ s !! 0), (charToShape $ s !! 2))

stringToRoundPart2 :: String -> Round
stringToRoundPart2 s = RoundPart2 ((charToShape $ s !! 0), (charToOutcome $ s !! 2))

main :: IO()
main = do
    inputFile <- readFile "../../inputs/02.txt"
    let run = (\stringToRound -> sum $ map calcRoundPts $ map stringToRound $ lines inputFile)
    let result1 = run stringToRoundPart1
    let result2 = run stringToRoundPart2
    print result1
    print result2
