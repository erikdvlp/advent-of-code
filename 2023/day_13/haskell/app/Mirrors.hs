module Mirrors where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import Grid
import Prelude hiding (getLine)

type Mirror = Grid
type Smudge = Bool

-- Converts given input file lines into mirrors.
linesToMirrors :: [String] -> [Mirror]
linesToMirrors lines = map linesToGrid chunks
  where
    chunks = splitOn [""] lines

-- Gets the number of characters that two given strings of equal length differ by.
getStringDiff :: String -> String -> Int
getStringDiff [] [] = 0
getStringDiff (x : xs) (y : ys)
    | x == y = getStringDiff xs ys
    | otherwise = 1 + getStringDiff xs ys

-- Checks whether the reflection around a given reflection line extends across the whole given mirror.
isFullReflection :: Mirror -> Int -> Int -> Int -> (Grid -> Int -> String) -> Smudge -> Bool
isFullReflection mirror i1 i2 iMax getFn smudge
    | i1 < 0 || i2 >= iMax = True
    | a == b = isFullReflection' smudge
    | smudge && (getStringDiff a b == 1) = isFullReflection' False
    | otherwise = False
  where
    a = getFn mirror i1
    b = getFn mirror i2
    isFullReflection' = isFullReflection mirror (i1 - 1) (i2 + 1) iMax getFn

-- Gets the reflection line of a given mirror.
findMirrorLine :: Mirror -> Int -> Int -> (Grid -> Int -> String) -> Smudge -> Maybe Int -> Maybe Int
findMirrorLine mirror i iMax getFn smudge avoid
    | (i + 1) >= iMax = Nothing
    | (line /= avoid) && (a == b) && isFullReflection' smudge = line
    | (line /= avoid) && smudge && (getStringDiff a b == 1) && isFullReflection' False = line
    | otherwise = findMirrorLine mirror (i + 1) iMax getFn smudge avoid
  where
    a = getFn mirror i
    b = getFn mirror (i + 1)
    line = Just (i + 1)
    isFullReflection' = isFullReflection mirror (i - 1) (i + 2) iMax getFn

-- Calculates the values of a given mirror with and without accounting for a smudge.
calcMirrorValues :: Mirror -> (Int, Int)
calcMirrorValues mirror = (a, b)
  where
    aH = findMirrorLine mirror 0 (gridHeight mirror) getRowFromGrid False Nothing
    aV = findMirrorLine mirror 0 (gridWidth mirror) (getColFromGrid 0) False Nothing
    bH = findMirrorLine mirror 0 (gridHeight mirror) getRowFromGrid True aH
    bV = findMirrorLine mirror 0 (gridWidth mirror) (getColFromGrid 0) True aV
    a = if isJust aH then fromJust aH * 100 else fromJust aV
    b = if isJust bH then fromJust bH * 100 else fromJust bV
