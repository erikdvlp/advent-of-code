module Main where

import Space (
    calcDistance,
    getEmptyCols,
    getEmptyRows,
    getGalaxyCoords,
    pairCoords,
 )

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let space = lines inputFile
    let emptyRows = getEmptyRows 0 space
    let emptyCols = getEmptyCols 0 space
    let galaxyPairs = pairCoords $ getGalaxyCoords (0, 0) space
    let run n = sum $ map (calcDistance n emptyRows emptyCols) galaxyPairs
    let result1 = run 1
    print ("Part 1 result: " ++ show result1)
    let result2 = run 999999
    print ("Part 2 result: " ++ show result2)
