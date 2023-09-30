module Main (main) where

type DataStream = String
type Index = Int
type Size = Int

-- Checks if a given part of a data stream is unique.
isUnique :: String -> Bool
isUnique [] = True
isUnique (x : xs) = x `notElem` xs && isUnique xs

-- Finds the index of a packet marker in a given data stream.
findMarker :: DataStream -> Index -> Size -> Maybe Index
findMarker ds i size
    | size > length ds = Nothing
    | otherwise =
        if isUnique (take size ds)
            then Just (i + size)
            else findMarker (tail ds) (i + 1) size

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let result1 = findMarker inputFile 0 4
    let result2 = findMarker inputFile 0 14
    print ("Part 1 result: " ++ show result1)
    print ("Part 2 result: " ++ show result2)
