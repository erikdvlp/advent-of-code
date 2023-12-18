module Lagoon where

import Data.Char (digitToInt, toUpper)
import Prelude hiding (Left, Right)

type Coords = (Int, Int)
data ProblemPart = Part1 | Part2

-- Parses a given input file line and converts it into a step.
lineToStep :: ProblemPart -> String -> Coords
lineToStep Part1 line
    | direction == "U" = (0, -magnitude)
    | direction == "D" = (0, magnitude)
    | direction == "L" = (-magnitude, 0)
    | otherwise = (magnitude, 0)
  where
    parts = words line
    direction = head parts
    magnitude = read $ parts !! 1
lineToStep Part2 line
    | direction == '3' = (0, -magnitude)
    | direction == '1' = (0, magnitude)
    | direction == '2' = (-magnitude, 0)
    | otherwise = (magnitude, 0)
  where
    hex = init $ drop 2 $ last $ words line
    direction = last hex
    hexToDecimal = sum . zipWith (*) (iterate (* 16) 1) . reverse . map (digitToInt . toUpper)
    magnitude = hexToDecimal $ init hex

-- Calculates the exterior area of a lagoon from given steps.
calcExterior :: Coords -> [Coords] -> [Coords]
calcExterior _ [] = []
calcExterior (x1, y1) ((x2, y2) : zs) = newCoords : calcExterior newCoords zs
  where
    newCoords = (x1 + x2, y1 + y2)

-- Calculates the interior area of a lagoon using the shoelace formula.
calcInterior :: [Coords] -> Int
calcInterior [(x1, y1), (x2, y2)] = x1 * y2 - x2 * y1
calcInterior ((x1, y1) : z@(x2, y2) : zs) = (x1 * y2 - x2 * y1) + calcInterior (z : zs)
