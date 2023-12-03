module Main where

import Engine (
    Number (isPart, numVal),
    Symbol (symVal),
    getGearRatio,
    getNumbersInSchematic,
    getSymbolsInSchematic,
    setIsPartNumber,
 )

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let nums = getNumbersInSchematic 0 0 $ lines inputFile
    let syms = getSymbolsInSchematic 0 0 $ lines inputFile
    let partNums = filter isPart $ map (setIsPartNumber syms) nums
    let result1 = sum $ map (read . numVal) partNums
    print ("Part 1 result: " ++ show result1)
    let result2 = sum $ map (getGearRatio partNums) $ filter (\x -> symVal x == '*') syms
    print ("Part 2 result: " ++ show result2)
