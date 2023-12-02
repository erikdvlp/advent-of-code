module Game where

import Data.List.Split (splitOn)

type GameID = Int
type Red = Int
type Green = Int
type Blue = Int
type CubeSet = (Red, Green, Blue)
data Game = Game {gameId :: GameID, rounds :: [CubeSet]}

-- Parses a given round part of an input file line and converts it to a round.
parseRoundLine :: CubeSet -> String -> CubeSet
parseRoundLine round [] = round
parseRoundLine (r, g, b) roundLine
    | take 3 (roundParts !! 1) == "red" = parseRoundLine (r + numOfCubes, g, b) (unwords $ drop 2 roundParts)
    | take 5 (roundParts !! 1) == "green" = parseRoundLine (r, g + numOfCubes, b) (unwords $ drop 2 roundParts)
    | otherwise = parseRoundLine (r, g, b + numOfCubes) (unwords $ drop 2 roundParts)
  where
    roundParts = splitOn " " roundLine
    numOfCubes = read $ head roundParts

-- Parses a given input file line and converts it to a game.
lineToGame :: String -> Game
lineToGame line = Game{gameId = gameId, rounds = rounds}
  where
    lineParts = splitOn ": " line
    gameId = read $ drop 5 $ head lineParts
    roundLines = splitOn "; " $ last lineParts
    rounds = map (parseRoundLine (0, 0, 0)) roundLines

-- Checks if a given round is possible given limited cubes for part 1 of the problem.
isRoundPossible :: Red -> Green -> Blue -> CubeSet -> Bool
isRoundPossible rMax gMax bMax (r, g, b) = (r <= rMax) && (g <= gMax) && (b <= bMax)

-- Checks if a given game is possible given limited cubes for part 1 of the problem.
isGamePossible :: Red -> Green -> Blue -> Game -> Bool
isGamePossible r g b game = all (isRoundPossible r g b) (rounds game)

-- Gets the value of a game, which is either equal to zero or the game's ID, depending on whether the game is possible, for part 1 of the problem.
getValueOfGame :: Red -> Green -> Blue -> Game -> Int
getValueOfGame r g b game =
    if isGamePossible r g b game
        then gameId game
        else 0

-- Gets the sum of the values of all possible games given limited cubes for part 1 of the problem.
getPossibleGames :: Red -> Green -> Blue -> [Game] -> Int
getPossibleGames r g b games = sum $ map (getValueOfGame r g b) games

-- Gets the minimum number of each cube required to play a game given a list of rounds for part 2 of the problem.
getMinCubesForRounds :: CubeSet -> [CubeSet] -> CubeSet
getMinCubesForRounds cubes [] = cubes
getMinCubesForRounds (r, g, b) rounds = getMinCubesForRounds (rMax, gMax, bMax) (drop 1 rounds)
  where
    (rCurr, gCurr, bCurr) = head rounds
    rMax = max r rCurr
    gMax = max g gCurr
    bMax = max b bCurr

-- Gets the minimum number of each cube required to play a game given a list of games for part 2 of the problem.
getMinCubesForGame :: [Game] -> [CubeSet]
getMinCubesForGame = map (getMinCubesForRounds (0, 0, 0) . rounds)

-- Calculates the power of a set of cubes for part 2 of the problem.
calcPowerOfCubes :: CubeSet -> Int
calcPowerOfCubes (r, g, b) = r * g * b
