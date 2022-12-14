module Helpers where

type AssignmentPair = ((Int, Int), (Int, Int))
data Overlap = None | Partial | Full deriving (Eq)

getOverlap :: AssignmentPair -> Overlap
getOverlap ((l1, l2), (r1, r2)) = if (l1 <= r1 && l2 >= r2) || (l1 >= r1 && l2 <= r2)
    then Full
    else if (l1 <= r1 && r1 <= l2) || (l1 <= r2 && r2 <= l2)
        then Partial
        else None
