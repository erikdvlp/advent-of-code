module Inventory where

import Data.List (sortBy)

type Calories = Int
type Inventory = [Calories]

-- Converts grouped input file lines into inventories.
linesToInventories :: [[String]] -> [Inventory]
linesToInventories = map (map read)

-- Calculates the caloric sum of each inventory given an array of inventories.
sumInventories :: [Inventory] -> [Calories]
sumInventories = map sum

-- Gets the top three calories given an array of calories.
getTopThreeInventories :: [Calories] -> [Calories]
getTopThreeInventories inventories = take 3 $ sortBy (flip compare) inventories
