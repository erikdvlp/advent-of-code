module Boats where

import Data.List (intercalate)

type Time = Int
type Distance = Int
type Race = (Time, Distance)

-- Parses given input file lines and converts them into races.
-- Only used for part 1 of the problem.
linesToRaces :: [String] -> [Race]
linesToRaces lines = zip times distances
  where
    readLine line = map read $ tail $ words line
    times = readLine $ head lines :: [Int]
    distances = readLine $ last lines :: [Int]

-- Parses given input file lines and converts them into a race.
-- Only used for part 2 of the problem.
linesToRace :: [String] -> Race
linesToRace lines = (time, distance)
  where
    readLine line = read $ intercalate "" $ tail $ words line
    time = readLine $ head lines :: Int
    distance = readLine $ last lines :: Int

-- Calculates the number of ways to win a given race.
calcWins :: Time -> Race -> Int
calcWins holdTime race@(maxTime, record)
    | holdTime < maxTime =
        if distance > record
            then 1 + calcWins (holdTime + 1) race
            else calcWins (holdTime + 1) race
    | otherwise = 0
  where
    distance = (maxTime - holdTime) * holdTime
