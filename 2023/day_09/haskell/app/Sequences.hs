module Sequences where

type Sequence = [Int]

-- Parses a given input file line and converts it to a sequence.
lineToSequence :: String -> [Int]
lineToSequence line = map read $ words line

-- Generate a sequence of differences from a given sequence.
genDiffSequence :: Sequence -> Sequence
genDiffSequence [x, y] = [x - y]
genDiffSequence (x : y : ys) = (x - y) : genDiffSequence (y : ys)

-- Generate all sequences of differences from a given sequence.
genDiffSequences :: Sequence -> [Sequence]
genDiffSequences s
    | all (== 0) s = [s]
    | otherwise = s : genDiffSequences (genDiffSequence s)

-- Pads each given sequence with a zero.
padSequences :: [[Sequence]] -> [[Sequence]]
padSequences = map (map padSequence)
  where
    padSequence (x : xs) = 0 : x : xs

-- Calculates the next values for each in a given list of sequences.
calcNextVals :: [Sequence] -> Sequence
calcNextVals a@(x : xs) = foldl calcNextVal x a
  where
    calcNextVal (x : xs) (y : z : zs) = x + z : z : zs
