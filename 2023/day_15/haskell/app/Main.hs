module Main where

import Data.List.Split (splitOn)
import qualified Data.Vector as V
import Initialization (Box, calcBoxesPower, parseStep, processStep, runHashAlgo)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let sequence = splitOn "," inputFile
    let result1 = sum $ map (runHashAlgo 0) sequence
    print ("Part 1 result: " ++ show result1)
    let steps = map parseStep sequence
    let emptyBoxes = V.fromList (replicate 256 ([] :: Box))
    let boxes = foldl processStep emptyBoxes steps
    let result2 = calcBoxesPower 1 (V.toList boxes)
    print ("Part 2 result: " ++ show result2)
