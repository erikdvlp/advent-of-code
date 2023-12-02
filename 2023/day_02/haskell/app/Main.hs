module Main where

import Game (calcPowerOfCubes, getMinCubesForGame, getPossibleGames, lineToGame)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let games = map lineToGame (lines inputFile)
    let result1 = getPossibleGames 12 13 14 games
    print ("Part 1 result: " ++ show result1)
    let result2 = sum $ map calcPowerOfCubes $ getMinCubesForGame games
    print ("Part 2 result: " ++ show result2)
