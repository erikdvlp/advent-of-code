module Main (main) where

isUnique :: [Char] -> Bool
isUnique [] = True
isUnique (x:xs) = x `notElem` xs && isUnique xs

findMarker :: String -> Int -> Int -> Int
findMarker s pointer size = if isUnique (take size s)
    then pointer + size
    else findMarker (tail s) (pointer + 1) size

main :: IO()
main = do
    inputFile <- readFile "../../inputs/06.txt"
    let run = (\size -> findMarker inputFile 0 size)
    let result1 = run 4
    let result2 = run 14
    print result1
    print result2
