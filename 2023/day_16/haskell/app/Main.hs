module Main where

import Beam (Direction (Right), calcStarts, traverse)
import Data.List (nub)
import Grid (linesToGrid)
import Prelude hiding (Right, traverse)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let grid = linesToGrid $ lines inputFile
    let result1 = length $ nub $ map fst $ traverse grid [] (0, 0) Right
    print ("Part 1 result: " ++ show result1)
    let result2 = maximum $ map ((length . nub . map fst) . uncurry (traverse grid [])) (calcStarts grid)
    print ("Part 2 result: " ++ show result2)
