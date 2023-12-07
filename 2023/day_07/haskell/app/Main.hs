module Main where

import Cards (
    Play (playId),
    ProblemPart (Part1, Part2),
    getPlayWinnings,
    linesToPlays,
    setPlayRanks,
 )
import Data.List (sort, sortOn)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let run part = sum $ map getPlayWinnings $ sortOn playId $ setPlayRanks 1 $ sort $ linesToPlays part 0 $ lines inputFile
    let result1 = run Part1
    print ("Part 1 result: " ++ show result1)
    let result2 = run Part2
    print ("Part 2 result: " ++ show result2)
