module Cycle where

-- Moves the hare into the cycle by moving the hare at twice the speed of the tortoise.
moveHareIntoCycle :: (Eq a) => [a] -> [a] -> [a]
moveHareIntoCycle _ [] = []
moveHareIntoCycle (x : xs) (_ : y : ys)
    | x == y = ys
    | otherwise = moveHareIntoCycle xs ys

-- Calculates the cycle's starting position by moving the tortoise and the hare (which is inside the cycle) at the same speed.
calcCycleStart :: (Eq a) => Int -> [a] -> [a] -> Int
calcCycleStart _ [] _ = 0
calcCycleStart i (x : xs) (y : ys)
    | x == y = i
    | otherwise = calcCycleStart (i + 1) xs ys

-- Calculates the cycle's length by moving the hare (which is inside the cycle) until it meets the tortoise (which is at the start of the cycle).
calcCycleLength :: (Eq a) => Int -> a -> [a] -> Int
calcCycleLength _ _ [] = 0
calcCycleLength i tortoise (x : xs)
    | x == tortoise = i
    | otherwise = calcCycleLength (i + 1) tortoise xs

-- Calculates the starting index and length of a cycle using Floyd's cycle-finding algorithm.
calcCycle :: (Eq a) => [a] -> (Int, Int)
calcCycle patterns = (cycleStart, cycleLength)
  where
    hare = moveHareIntoCycle patterns patterns
    cycleStart = calcCycleStart 0 patterns hare
    cycleLength = calcCycleLength 1 (patterns !! cycleStart) (drop (cycleStart + 1) patterns)
