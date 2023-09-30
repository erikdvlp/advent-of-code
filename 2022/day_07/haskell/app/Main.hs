module Main where

import Command
import FileSystem

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let (fileSystem, _) = processCommands (Directory "/" 0 [], []) $ lines inputFile
    let result1 = sumDirsBelow100K 0 fileSystem
    let spaceNeeded = 30000000 - (70000000 - fileSize fileSystem)
    let result2 = dirToDeleteSize spaceNeeded (fileSize fileSystem) fileSystem
    print ("Part 1 result: " ++ show result1)
    print ("Part 2 result: " ++ show result2)
