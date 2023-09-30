module Items where

import Data.Char (isAsciiLower, ord)
import Data.List (intersect)

type Item = Char
type Priority = Int

data ItemContainer = Rucksack (String, String) | ElfGroup (String, String, String)

-- Parses a given input file line and converts it into a rucksack for part 1 of the problem.
lineToRucksack :: String -> ItemContainer
lineToRucksack s = Rucksack $ splitAt ((length s + 1) `div` 2) s

-- Parses a given array of grouped input file lines and converts them into an elf group for part 2 of the problem.
linesToElfGroup :: [String] -> ItemContainer
linesToElfGroup s = ElfGroup (head s, s !! 1, s !! 2)

-- Gets the first item in common in a rucksack or an elf group.
getCommonItem :: ItemContainer -> Item
getCommonItem (Rucksack (x, y)) = head (x `intersect` y)
getCommonItem (ElfGroup (x, y, z)) = head (x `intersect` (y `intersect` z))

-- Calculates the priority of an item.
getItemPriority :: Item -> Priority
getItemPriority item
    | isAsciiLower item = ord item - ord 'a' + 1
    | otherwise = ord item - ord 'A' + 27
