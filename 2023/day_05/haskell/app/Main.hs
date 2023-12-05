module Main where

import Almanac (
    ProblemPart (Part1),
    calcMinLoc,
    createAlmanacEntry,
    getAllMapsFromLines,
    getMinLoc,
    lineToSeedRange,
 )

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let maps = getAllMapsFromLines $ lines inputFile
    let almanac = map (createAlmanacEntry Part1 maps . read) (tail $ words $ head $ lines inputFile)
    let result1 = getMinLoc almanac (maxBound :: Int)
    print ("Part 1 result: " ++ show result1)
    let seedRange = lineToSeedRange $ tail $ words $ head $ lines inputFile
    let result2 = calcMinLoc seedRange maps 0
    print ("Part 2 result: " ++ show result2)
