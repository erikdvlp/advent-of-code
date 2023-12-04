module Cards where

import Data.List.Split (splitOn)

type Numbers = [String]
type Card = (Numbers, Numbers)
type Copies = [Int]

-- Parses a given input file line and converts it to a card.
lineToCard :: String -> Card
lineToCard line = (winningNums, availableNums)
  where
    allNums = splitOn "|" $ last $ splitOn ": " line
    winningNums = filter (/= "") (splitOn " " $ head allNums)
    availableNums = filter (/= "") (splitOn " " $ last allNums)

-- Calculate points for a given card for part 1 of the problem.
calcPointsOfCard :: Int -> Card -> Int
calcPointsOfCard points ([], _) = points
calcPointsOfCard points (x : xs, nums) =
    if x `elem` nums
        then calcPointsOfCard newPoints (xs, nums)
        else calcPointsOfCard points (xs, nums)
  where
    newPoints =
        if points == 0
            then 1
            else points * 2

-- Get the number of matches in a given card for part 2 of the problem.
getNumOfMatches :: Card -> Int
getNumOfMatches ([], _) = 0
getNumOfMatches (x : xs, nums) =
    if x `elem` nums
        then 1 + getNumOfMatches (xs, nums)
        else getNumOfMatches (xs, nums)

-- Modify given copies by adding more copies between a given index range for part 2 of the problem.
addCopies :: Copies -> Int -> Int -> Int -> Int -> Copies
addCopies [] _ _ _ _ = []
addCopies (x : xs) i j k val =
    if i >= j && i < k
        then (x + val) : addCopies xs (i + 1) j k val
        else x : addCopies xs (i + 1) j k val

-- Calculate the number of copies of given cards for part 2 of the problem.
calcCopiesOfCards :: Copies -> Int -> [Card] -> Copies
calcCopiesOfCards copies _ [] = copies
calcCopiesOfCards copies i (x : xs) = calcCopiesOfCards newCopies (i + 1) xs
  where
    numOfMatches = getNumOfMatches x
    newCopies = addCopies copies 0 (i + 1) (i + 1 + numOfMatches) (copies !! i)
