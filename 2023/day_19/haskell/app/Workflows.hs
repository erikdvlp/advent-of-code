module Workflows where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)

type Rule = (Maybe Char, Maybe Char, Maybe Int, String)
type Workflows = Map String [Rule]
data Part = Part {partX :: Int, partM :: Int, partA :: Int, partS :: Int}
type Bounds = ((Int, Int), (Int, Int), (Int, Int), (Int, Int))

-- Parses a given string containing rule data and converts it into a rule.
stringToRule :: String -> Rule
stringToRule s
    | ':' `notElem` s = (Nothing, Nothing, Nothing, s)
    | otherwise =
        let
            cat = head s
            com = s !! 1
            val = read $ head $ splitOn ":" $ drop 2 s
            dst = last $ splitOn ":" $ drop 2 s
         in
            (Just cat, Just com, Just val, dst)

-- Parses a given input file line containing workflow data and updates a map of workflows.
updateWorkflows :: Workflows -> [String] -> Workflows
updateWorkflows workflows [] = workflows
updateWorkflows workflows (x : xs) = updateWorkflows updated xs
  where
    parts = splitOn "{" $ init x
    location = head parts
    rules = map stringToRule $ splitOn "," $ concat $ tail parts
    updated = Map.insert location rules workflows

-- Parses a given input file line containing part data and converts it into a part.
lineToPart :: String -> Part
lineToPart line = Part{partX = x, partM = m, partA = a, partS = s}
  where
    parts = splitOn "," $ init $ drop 1 line
    x = read $ drop 2 $ head parts
    m = read $ drop 2 $ parts !! 1
    a = read $ drop 2 $ parts !! 2
    s = read $ drop 2 $ last parts

-- Gets the next workflow to follow for a given part.
-- Only used in part 1 of the problem.
getNextFromRules part ((Nothing, Nothing, Nothing, dst) : _) = dst
getNextFromRules part ((Just 'x', Just '<', Just val, dst) : xs)
    | partX part < val = dst
    | otherwise = getNextFromRules part xs
getNextFromRules part ((Just 'x', Just '>', Just val, dst) : xs)
    | partX part > val = dst
    | otherwise = getNextFromRules part xs
getNextFromRules part ((Just 'm', Just '<', Just val, dst) : xs)
    | partM part < val = dst
    | otherwise = getNextFromRules part xs
getNextFromRules part ((Just 'm', Just '>', Just val, dst) : xs)
    | partM part > val = dst
    | otherwise = getNextFromRules part xs
getNextFromRules part ((Just 'a', Just '<', Just val, dst) : xs)
    | partA part < val = dst
    | otherwise = getNextFromRules part xs
getNextFromRules part ((Just 'a', Just '>', Just val, dst) : xs)
    | partA part > val = dst
    | otherwise = getNextFromRules part xs
getNextFromRules part ((Just 's', Just '<', Just val, dst) : xs)
    | partS part < val = dst
    | otherwise = getNextFromRules part xs
getNextFromRules part ((Just 's', Just '>', Just val, dst) : xs)
    | partS part > val = dst
    | otherwise = getNextFromRules part xs

-- Checks whether a given part is accepted in a given workflow.
-- Only used in part 1 of the problem.
isPartAccepted :: Workflows -> String -> Part -> Bool
isPartAccepted workflows location part
    | dst == "A" = True
    | dst == "R" = False
    | otherwise = isPartAccepted workflows dst part
  where
    rules = fromJust $ Map.lookup location workflows
    dst = getNextFromRules part rules

-- Calculates the score of a given part.
-- Only used in part 1 of the problem.
getPartScore :: Workflows -> Part -> Int
getPartScore workflows part
    | isPartAccepted workflows "in" part = partX part + partM part + partA part + partS part
    | otherwise = 0

-- Adjusts a given set of bounds to account for a given workflow rule.
-- Only used in part 2 of the problem.
adjustBounds :: Bounds -> Maybe Char -> Maybe Char -> Maybe Int -> (Bounds, Bounds)
adjustBounds limits Nothing Nothing Nothing = (limits, limits)
adjustBounds limits@((x1, x2), m, a, s) (Just 'x') (Just '<') (Just val) = (((x1, val - 1), m, a, s), ((val, x2), m, a, s))
adjustBounds limits@((x1, x2), m, a, s) (Just 'x') (Just '>') (Just val) = (((val + 1, x2), m, a, s), ((x1, val), m, a, s))
adjustBounds limits@(x, (m1, m2), a, s) (Just 'm') (Just '<') (Just val) = ((x, (m1, val - 1), a, s), (x, (val, m2), a, s))
adjustBounds limits@(x, (m1, m2), a, s) (Just 'm') (Just '>') (Just val) = ((x, (val + 1, m2), a, s), (x, (m1, val), a, s))
adjustBounds limits@(x, m, (a1, a2), s) (Just 'a') (Just '<') (Just val) = ((x, m, (a1, val - 1), s), (x, m, (val, a2), s))
adjustBounds limits@(x, m, (a1, a2), s) (Just 'a') (Just '>') (Just val) = ((x, m, (val + 1, a2), s), (x, m, (a1, val), s))
adjustBounds limits@(x, m, a, (s1, s2)) (Just 's') (Just '<') (Just val) = ((x, m, a, (s1, val - 1)), (x, m, a, (val, s2)))
adjustBounds limits@(x, m, a, (s1, s2)) (Just 's') (Just '>') (Just val) = ((x, m, a, (val + 1, s2)), (x, m, a, (s1, val)))
adjustBounds limits _ _ _ = (limits, limits)

-- Calculates the minimum and maximum XMAS values that are required for a part to enter a given workflow.
-- Only used in part 2 of the problem.
calcBoundsLayer :: [Rule] -> Bounds -> [(String, Bounds)]
calcBoundsLayer [] _ = []
calcBoundsLayer ((cat, com, val, dst) : xs) limits = (dst, currLimits) : calcBoundsLayer xs nextLimits
  where
    (currLimits, nextLimits) = adjustBounds limits cat com val

-- Calculates the bounds for a part to qualify in all given workflows.
-- Only used in part 2 of the problem.
calcBounds :: Workflows -> String -> Bounds -> [(String, Bounds)]
calcBounds workflows location limits = layer ++ concatMap (uncurry (calcBounds workflows)) layer
  where
    rules = Map.lookup location workflows
    layer = if isJust rules then calcBoundsLayer (fromJust rules) limits else []

-- Calculates all possible distinct combinations of part values from given bounds.
-- Only used in part 2 of the problem.
calcCombinations :: [(String, Bounds)] -> Int
calcCombinations bounds = sum $ map ((\((a, b), (c, d), (e, f), (g, h)) -> (b - a + 1) * (d - c + 1) * (f - e + 1) * (h - g + 1)) . snd) $ filter (\(a, _) -> a == "A") bounds
