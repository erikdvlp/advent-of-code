module Assignment where

import Data.List.Split (splitOn)

type Section = Int
type AssignmentPair = ((Section, Section), (Section, Section))

data Overlap = None | Partial | Full deriving (Eq)

-- Parses a given input file line and converts it into an assignment pair.
lineToAssignmentPair :: String -> AssignmentPair
lineToAssignmentPair s = ((head a1, a1 !! 1), (head a2, a2 !! 1))
  where
    stringByElf = splitOn [','] s
    a1 = stringByElfToAssignment 0
    a2 = stringByElfToAssignment 1
    stringByElfToAssignment i = map read $ splitOn ['-'] (stringByElf !! i)

-- Gets an overlap for a given assignment pair.
getOverlap :: AssignmentPair -> Overlap
getOverlap ((w, x), (y, z))
    | (w <= y && x >= z) || (w >= y && x <= z) = Full
    | (w <= y && y <= x) || (w <= z && z <= x) = Partial
    | otherwise = None
