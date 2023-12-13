module Main where

import Mirrors (calcMirrorValues, linesToMirrors)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let mirrors = linesToMirrors $ lines inputFile
    let mirrorLines = map calcMirrorValues mirrors
    let result1 = sum $ map fst mirrorLines
    print ("Part 1 result: " ++ show result1)
    let result2 = sum $ map snd mirrorLines
    print ("Part 2 result: " ++ show result2)
