module Main where

import Cards (calcCopiesOfCards, calcPointsOfCard, lineToCard)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let cards = map lineToCard $ lines inputFile
    let result1 = sum $ map (calcPointsOfCard 0) cards
    print ("Part 1 result: " ++ show result1)
    let result2 = sum $ calcCopiesOfCards (replicate (length cards) 1) 0 cards
    print ("Part 2 result: " ++ show result2)
