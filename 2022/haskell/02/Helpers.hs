module Helpers where

data Shape = Paper | Rock | Scissors
data Outcome = Lose | Draw | Win
data Round = RoundPart1 (Shape, Shape)
    | RoundPart2 (Shape, Outcome)

charToShape :: Char -> Shape
charToShape 'A' = Rock
charToShape 'X' = Rock
charToShape 'B' = Paper
charToShape 'Y' = Paper
charToShape 'C' = Scissors
charToShape 'Z' = Scissors

charToOutcome :: Char -> Outcome
charToOutcome 'X' = Lose
charToOutcome 'Y' = Draw
charToOutcome 'Z' = Win

roundToOutcome :: Round -> Outcome
roundToOutcome (RoundPart1 (Rock, Paper)) = Win
roundToOutcome (RoundPart1 (Rock, Scissors)) = Lose
roundToOutcome (RoundPart1 (Paper, Rock)) = Lose
roundToOutcome (RoundPart1 (Paper, Scissors)) = Win
roundToOutcome (RoundPart1 (Scissors, Rock)) = Win
roundToOutcome (RoundPart1 (Scissors, Paper)) = Lose
roundToOutcome (RoundPart1 (_, _)) = Draw

roundToShape :: Round -> Shape
roundToShape (RoundPart2 (Rock, Lose)) = Scissors
roundToShape (RoundPart2 (Rock, Win)) = Paper
roundToShape (RoundPart2 (Paper, Lose)) = Rock
roundToShape (RoundPart2 (Paper, Win)) = Scissors
roundToShape (RoundPart2 (Scissors, Lose)) = Paper
roundToShape (RoundPart2 (Scissors, Win)) = Rock
roundToShape (RoundPart2 (s, Draw)) = s

outcomeToPts :: Outcome -> Int
outcomeToPts Lose = 0
outcomeToPts Draw = 3
outcomeToPts Win = 6

playToPts :: Shape -> Int
playToPts Rock = 1
playToPts Paper = 2
playToPts Scissors = 3

calcRoundPts :: Round -> Int
calcRoundPts (RoundPart1 round) = outcomeToPts (roundToOutcome (RoundPart1 round)) + playToPts (snd round)
calcRoundPts (RoundPart2 round) = outcomeToPts (snd round) + playToPts (roundToShape (RoundPart2 round))
