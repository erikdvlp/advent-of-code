module Main where

import Grid (findInGrid, linesToGrid)
import Pipes

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let grid = linesToGrid $ lines inputFile
    let path = findPath grid [] (Pipe{pipeType = 'S', coords = findInGrid grid 'S' (0, 0)})
    let result1 = length path `div` 2
    print ("Part 1 result: " ++ show result1)
    let result2 = countInsidePath grid path (0, 0)
    print ("Part 2 result: " ++ show result2)
