module Main where

import Lagoon (ProblemPart (..), calcExterior, calcInterior, lineToStep)

calcArea :: ProblemPart -> [String] -> Int
calcArea part lines = ((interior + exterior) `div` 2) + 1
  where
    steps = map (lineToStep part) lines
    exterior = sum $ map (abs . uncurry (+)) steps
    interior = calcInterior $ calcExterior (0, 0) steps

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let result1 = calcArea Part1 $ lines inputFile
    print ("Part 1 result: " ++ show result1)
    let result2 = calcArea Part2 $ lines inputFile
    print ("Part 2 result: " ++ show result2)
