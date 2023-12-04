module Engine where

import Data.Char (isDigit)

type Row = String
type Schematic = [Row]
data Number = Number {numVal :: String, numX1 :: Int, numX2 :: Int, numY :: Int, isPart :: Bool}
data Symbol = Symbol {symVal :: Char, symX :: Int, symY :: Int}

-- Parses a given input file line and extracts a list of numbers.
getNumbersInRow :: Int -> Int -> Maybe Number -> Row -> [Number]
getNumbersInRow _ _ Nothing [] = []
getNumbersInRow _ _ (Just curr) [] = [curr]
getNumbersInRow x y Nothing (z : zs)
    | isDigit z = getNumbersInRow (x + 1) y newNum zs
    | otherwise = getNumbersInRow (x + 1) y Nothing zs
  where
    newNum = Just Number{numVal = [z], numX1 = x, numX2 = x, numY = y, isPart = False}
getNumbersInRow x y (Just curr) (z : zs)
    | isDigit z = getNumbersInRow (x + 1) y newNum zs
    | otherwise = curr : getNumbersInRow (x + 1) y Nothing zs
  where
    newNum = Just Number{numVal = numVal curr ++ [z], numX1 = numX1 curr, numX2 = x, numY = y, isPart = False}

-- Parses a given list of input file lines and extracts a list of numbers.
getNumbersInSchematic :: Int -> Int -> Schematic -> [Number]
getNumbersInSchematic _ _ [] = []
getNumbersInSchematic x y s = getNumbersInRow x y Nothing (head s) ++ getNumbersInSchematic x (y + 1) (drop 1 s)

-- Checks whether a given character is a valid symbol.
isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

-- Parses a given input file line and extracts a list of symbols.
getSymbolsInRow :: Int -> Int -> Row -> [Symbol]
getSymbolsInRow _ _ [] = []
getSymbolsInRow x y (z : zs) =
    if isSymbol z
        then newSym : getSymbolsInRow (x + 1) y zs
        else getSymbolsInRow (x + 1) y zs
  where
    newSym = Symbol{symVal = z, symX = x, symY = y}

-- Parses a given list of input file lines and extracts a list of symbols.
getSymbolsInSchematic :: Int -> Int -> Schematic -> [Symbol]
getSymbolsInSchematic _ _ [] = []
getSymbolsInSchematic x y s = getSymbolsInRow x y (head s) ++ getSymbolsInSchematic x (y + 1) (drop 1 s)

-- Determines whether two positions are adjacent.
isAdjacent :: Int -> Int -> Int -> Int -> Int -> Bool
isAdjacent aX1 aX2 aY bX bY = ((abs (aX1 - bX) <= 1) || (abs (aX2 - bX) <= 1)) && (abs (aY - bY) <= 1)

-- Determines whether a given number is adjacent to any symbol in a given list of symbols for part 1 of the problem.
isAdjacentToAnySymbol :: [Symbol] -> Number -> Bool
isAdjacentToAnySymbol [] _ = False
isAdjacentToAnySymbol (x : xs) num = isAdjacent (numX1 num) (numX2 num) (numY num) (symX x) (symY x) || isAdjacentToAnySymbol xs num

-- Sets whether a given number is a part number by comparing it to a given list of symbols for part 1 of the problem.
setIsPartNumber :: [Symbol] -> Number -> Number
setIsPartNumber syms num = Number{numVal = numVal num, numX1 = numX1 num, numX2 = numX2 num, numY = numY num, isPart = isAdjacentToAnySymbol syms num}

-- Gets all numbers that are adjacent to a given symbol for part 2 of the problem.
getNumbersAdjacentToSymbol :: [Number] -> Symbol -> [Number]
getNumbersAdjacentToSymbol [] _ = []
getNumbersAdjacentToSymbol (x : xs) sym =
    if isAdjacent (numX1 x) (numX2 x) (numY x) (symX sym) (symY sym)
        then x : getNumbersAdjacentToSymbol xs sym
        else getNumbersAdjacentToSymbol xs sym

-- Gets the gear ratio of a given symbol for part 2 of the problem; can be zero if the symbol is not a gear.
getGearRatio :: [Number] -> Symbol -> Int
getGearRatio nums sym
    | length adjacentNums == 2 = read (numVal $ head adjacentNums) * read (numVal $ adjacentNums !! 1)
    | otherwise = 0
  where
    adjacentNums = getNumbersAdjacentToSymbol nums sym
