module Network where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (Left, Right)

data Instruction = Left | Right deriving (Eq)
type Node = String
type NodeMap = Map String (String, String)
data ProblemPart = Part1 | Part2

-- Parses a given input file line containing instruction data and converts it to a list of instructions.
lineToInstructions :: String -> [Instruction]
lineToInstructions [] = []
lineToInstructions (x : xs)
    | x == 'L' = Left : lineToInstructions xs
    | x == 'R' = Right : lineToInstructions xs
    | otherwise = error "Received invalid instruction"

-- Parses a given input file line containing map data and converts it into a tuple of paths.
lineToPaths :: String -> (String, (String, String))
lineToPaths line = (a, (b, c))
  where
    parts1 = splitOn " = " line
    a = head parts1
    parts2 = splitOn ", " $ last parts1
    b = drop 1 $ head parts2
    c = take 3 $ last parts2

-- Parses given input file lines containing map data and converts them into a node map.
linesToNodeMap :: [String] -> NodeMap
linesToNodeMap lines = Map.fromList paths
  where
    paths = map lineToPaths $ drop 2 lines

-- Unwraps an optional pair of nodes; throws an error if nothing.
unwrapNodes :: Maybe (Node, Node) -> (Node, Node)
unwrapNodes Nothing = error "Received invalid node"
unwrapNodes (Just nodes) = nodes

-- Calculates the number of steps that it takes to reach the destination with a given node map and given instructions.
calcStepsToDest :: ProblemPart -> NodeMap -> [Instruction] -> Node -> Int
calcStepsToDest Part1 _ _ "ZZZ" = 0
calcStepsToDest Part2 _ _ [_, _, 'Z'] = 0
calcStepsToDest part nodeMap (x : xs) curr = 1 + calcStepsToDest part nodeMap xs next
  where
    next =
        if x == Left
            then fst $ unwrapNodes $ Map.lookup curr nodeMap
            else snd $ unwrapNodes $ Map.lookup curr nodeMap

-- Gets all nodes ending in A from a given node map.
-- Only used for part 2 of the problem.
getNodesEndingInA :: NodeMap -> [Node]
getNodesEndingInA nodeMap = filter (\[_, _, x] -> x == 'A') $ Map.keys nodeMap
