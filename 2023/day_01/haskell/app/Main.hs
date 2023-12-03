module Main where

import Data.Char (isDigit)

-- Replaces all string digits in a given input line with numerical digits.
lineConvertStringDigits :: String -> String
lineConvertStringDigits [] = []
lineConvertStringDigits line
    | take 3 line == "one" = '1' : lineConvertStringDigits (drop 1 line)
    | take 3 line == "two" = '2' : lineConvertStringDigits (drop 1 line)
    | take 3 line == "six" = '6' : lineConvertStringDigits (drop 1 line)
    | take 4 line == "four" = '4' : lineConvertStringDigits (drop 1 line)
    | take 4 line == "five" = '5' : lineConvertStringDigits (drop 1 line)
    | take 4 line == "nine" = '9' : lineConvertStringDigits (drop 1 line)
    | take 5 line == "three" = '3' : lineConvertStringDigits (drop 1 line)
    | take 5 line == "seven" = '7' : lineConvertStringDigits (drop 1 line)
    | take 5 line == "eight" = '8' : lineConvertStringDigits (drop 1 line)
    | otherwise = head line : lineConvertStringDigits (drop 1 line)

-- Parses a given input file line and gets the calibration value.
lineToCalibrationValue :: Maybe Char -> Maybe Char -> String -> Int
lineToCalibrationValue Nothing Nothing [] = 0
lineToCalibrationValue (Just first) (Just last) [] = read [first, last]
lineToCalibrationValue Nothing Nothing (x : xs) =
    if isDigit x
        then lineToCalibrationValue (Just x) (Just x) xs
        else lineToCalibrationValue Nothing Nothing xs
lineToCalibrationValue (Just first) (Just last) (x : xs) =
    if isDigit x
        then lineToCalibrationValue (Just first) (Just x) xs
        else lineToCalibrationValue (Just first) (Just last) xs

main :: IO ()
main = do
    inputFile <- readFile "../input.txt"
    let result1 = sum $ map (lineToCalibrationValue Nothing Nothing) (lines inputFile)
    print ("Part 1 result: " ++ show result1)
    let convertedLines = map lineConvertStringDigits (lines inputFile)
    let result2 = sum $ map (lineToCalibrationValue Nothing Nothing) convertedLines
    print ("Part 2 result: " ++ show result2)
