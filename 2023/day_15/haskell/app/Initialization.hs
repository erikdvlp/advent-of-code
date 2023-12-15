module Initialization where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Vector as V

data Operation = Add | Remove deriving (Eq)
data Lens = Lens {label :: String, focalLength :: Maybe Int}
type Step = (Int, Operation, Lens)
type Box = [Lens]
type Boxes = V.Vector Box

instance Eq Lens where
    (Lens aLabel _) == (Lens bLabel _) = aLabel == bLabel

-- Returns the result of running the HASH algorithm on a given string.
runHashAlgo :: Int -> String -> Int
runHashAlgo value [] = value
runHashAlgo value (x : xs) = runHashAlgo newValue xs
  where
    newValue = ((value + ord x) * 17) `mod` 256

-- Parses a given part of an initialization sequence and converts it into a step.
parseStep :: String -> Step
parseStep s
    | '=' `elem` s =
        let parts = splitOn "=" s
         in (runHashAlgo 0 (head parts), Add, Lens{label = head parts, focalLength = Just (read (last parts) :: Int)})
    | otherwise = (runHashAlgo 0 (init s), Remove, Lens{label = init s, focalLength = Nothing})

-- Replaces a given lens in a given box.
replaceLensInBox :: Box -> Lens -> Box
replaceLensInBox (x : xs) lens
    | x == lens = lens : xs
    | otherwise = x : replaceLensInBox xs lens

-- Adds a given lens to a given box.
addLensToBox :: Box -> Lens -> Box
addLensToBox box lens
    | lens `elem` box = replaceLensInBox box lens
    | otherwise = lens : box

-- Removes a given lens from a given box.
removeLensFromBox :: Box -> Lens -> Box
removeLensFromBox [] lens = []
removeLensFromBox (x : xs) lens
    | x == lens = removeLensFromBox xs lens
    | otherwise = x : removeLensFromBox xs lens

-- Processes a given step by adding, replacing, or removing lenses.
processStep :: Boxes -> Step -> Boxes
processStep boxes (i, op, lens) = boxes V.// [(i, newBox)]
  where
    newBox =
        if op == Add
            then addLensToBox (boxes V.! i) lens
            else removeLensFromBox (boxes V.! i) lens

-- Calculates the power of the lens in given boxes.
calcBoxesPower :: Int -> [Box] -> Int
calcBoxesPower _ [] = 0
calcBoxesPower i (x : xs) = calcBoxPower i (length x) x + calcBoxesPower (i + 1) xs
  where
    calcLensPower i j lens = i * j * fromJust (focalLength lens)
    calcBoxPower _ _ [] = 0
    calcBoxPower i j (x : xs) = calcLensPower i j x + calcBoxPower i (j - 1) xs
