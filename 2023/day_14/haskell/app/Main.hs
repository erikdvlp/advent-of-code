{-# LANGUAGE NumericUnderscores #-}

module Main where

import Cycle (calcCycle)
import Data.List (transpose)
import Rocks (Direction (Left), calcLoad, shiftRow, spinPlatform)
import Prelude hiding (Left)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let platform = lines inputFile
    let result1 = calcLoad (length platform) $ transpose $ map (shiftRow Left) $ transpose platform
    print ("Part 1 result: " ++ show result1)
    let spunPlatforms = spinPlatform platform 500
    let (cycleStart, cycleLength) = calcCycle spunPlatforms
    let billionCycles = cycleStart + (1_000_000_000 - cycleStart) `mod` cycleLength
    let result2 = calcLoad (length platform) (spunPlatforms !! (billionCycles - 1))
    print ("Part 2 result: " ++ show result2)
