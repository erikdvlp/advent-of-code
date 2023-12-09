module Main where

import Sequences (
    calcNextVals,
    genDiffSequences,
    lineToSequence,
    padSequences,
 )

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let result1 = sum $ map (head . calcNextVals) $ padSequences $ map ((reverse . genDiffSequences) . reverse . lineToSequence) $ lines inputFile
    print ("Part 1 result: " ++ show result1)
    let result2 = sum $ map (head . calcNextVals) $ padSequences $ map (genDiffSequences . lineToSequence) $ lines inputFile
    print ("Part 2 result: " ++ show result2)
