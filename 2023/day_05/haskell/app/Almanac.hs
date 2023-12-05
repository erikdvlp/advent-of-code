module Almanac where

data AlmanacEntry = AlmanacEntry {seed :: Int, soil :: Int, fert :: Int, water :: Int, light :: Int, temp :: Int, hum :: Int, loc :: Int}
type Almanac = [AlmanacEntry]
type MapEntry = (Int, Int, Int)
type Map = [MapEntry]
type SeedRange = [(Int, Int)]
data ProblemPart = Part1 | Part2

-- Parses given input file lines and converts them into a map of the given type.
getMapFromLines :: [String] -> String -> Bool -> Map
getMapFromLines [] _ _ = []
getMapFromLines (x : xs) mapType False =
    if x == mapType
        then getMapFromLines xs mapType True
        else getMapFromLines xs mapType False
getMapFromLines (x : xs) mapType True =
    if x == ""
        then []
        else (read (head parts), read (parts !! 1), read (parts !! 2)) : getMapFromLines xs mapType True
  where
    parts = words x

-- Parses given input file lines and converts them into maps.
getAllMapsFromLines :: [String] -> [Map]
getAllMapsFromLines lines =
    [ getMapFromLines lines "seed-to-soil map:" False
    , getMapFromLines lines "soil-to-fertilizer map:" False
    , getMapFromLines lines "fertilizer-to-water map:" False
    , getMapFromLines lines "water-to-light map:" False
    , getMapFromLines lines "light-to-temperature map:" False
    , getMapFromLines lines "temperature-to-humidity map:" False
    , getMapFromLines lines "humidity-to-location map:" False
    ]

-- Gets a destination value given a source value and a map.
getDestFromMap :: ProblemPart -> Map -> Int -> Int
getDestFromMap _ [] val = val
getDestFromMap Part1 ((a, b, len) : xs) val =
    if val >= b && val < (b + len)
        then val + a - b
        else getDestFromMap Part1 xs val
getDestFromMap Part2 ((a, b, len) : xs) val =
    if val >= a && val < (a + len)
        then val - a + b
        else getDestFromMap Part2 xs val

-- Gets the lowest location number from a given alamanac.
-- Only used for part 1 of the problem.
getMinLoc :: Almanac -> Int -> Int
getMinLoc [] minLoc = minLoc
getMinLoc (x : xs) minLoc = getMinLoc xs newMinLoc
  where
    newMinLoc = min minLoc (loc x)

-- Create an almanac entry from a given seed or location value and given maps.
createAlmanacEntry :: ProblemPart -> [Map] -> Int -> AlmanacEntry
createAlmanacEntry Part1 maps seedVal = AlmanacEntry{seed = seedVal, soil = soilVal, fert = fertVal, water = waterVal, light = lightVal, temp = tempVal, hum = humVal, loc = locVal}
  where
    soilVal = getDestFromMap Part1 (head maps) seedVal
    fertVal = getDestFromMap Part1 (maps !! 1) soilVal
    waterVal = getDestFromMap Part1 (maps !! 2) fertVal
    lightVal = getDestFromMap Part1 (maps !! 3) waterVal
    tempVal = getDestFromMap Part1 (maps !! 4) lightVal
    humVal = getDestFromMap Part1 (maps !! 5) tempVal
    locVal = getDestFromMap Part1 (maps !! 6) humVal
createAlmanacEntry Part2 maps locVal = AlmanacEntry{seed = seedVal, soil = soilVal, fert = fertVal, water = waterVal, light = lightVal, temp = tempVal, hum = humVal, loc = locVal}
  where
    humVal = getDestFromMap Part2 (maps !! 6) locVal
    tempVal = getDestFromMap Part2 (maps !! 5) humVal
    lightVal = getDestFromMap Part2 (maps !! 4) tempVal
    waterVal = getDestFromMap Part2 (maps !! 3) lightVal
    fertVal = getDestFromMap Part2 (maps !! 2) waterVal
    soilVal = getDestFromMap Part2 (maps !! 1) fertVal
    seedVal = getDestFromMap Part2 (head maps) soilVal

-- Parses a given input file line containing seed data and converts it to a seed range.
-- Only used for part 2 of the problem.
lineToSeedRange :: [String] -> SeedRange
lineToSeedRange [] = []
lineToSeedRange (x : y : ys) = (read x, read x + read y) : lineToSeedRange ys

-- Checks if given a seed value falls within a given seed range.
-- Only used for part 2 of the problem.
isValidSeed :: Int -> SeedRange -> Bool
isValidSeed _ [] = False
isValidSeed seedVal ((a, b) : xs) = (seedVal >= a && seedVal < b) || isValidSeed seedVal xs

-- Calculates the lowest location number by brute forcing location numbers and then checking if they match a valid seed number.
-- Only used for part 2 of the problem.
calcMinLoc :: SeedRange -> [Map] -> Int -> Int
calcMinLoc range maps minLoc =
    if isValidSeed seedVal range
        then minLoc
        else calcMinLoc range maps (minLoc + 1)
  where
    seedVal = seed $ createAlmanacEntry Part2 maps minLoc
