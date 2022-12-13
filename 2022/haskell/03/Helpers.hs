module Helpers where

import Data.Char (ord)
import Data.List (intersect)

type Item = Char
data ItemContainer = Rucksack (String, String)
    | ElfGroup (String, String, String)

getCommonItem :: ItemContainer -> Item
getCommonItem (Rucksack (c1, c2)) = (intersect c1 c2) !! 0
getCommonItem (ElfGroup (c1, c2, c3)) = (intersect c1 $ intersect c2 c3) !! 0

calcPriority :: Item -> Int
calcPriority item
    | item >= 'a' && item <= 'z' = ord item - ord 'a' + 1
    | otherwise = ord item - ord 'A' + 27
