module Main where

import Trees (treesVisible)

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let result1 = treesVisible (lines inputFile) (0, 0) 0
    print ("Part 1 result: " ++ show result1)
