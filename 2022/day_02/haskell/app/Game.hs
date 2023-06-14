module Game where

type Points = Int

data Shape = Paper | Rock | Scissors
data Outcome = Lose | Draw | Win
data Round = RoundPart1 (Shape, Shape) | RoundPart2 (Shape, Outcome)

-- Parses a given input file line and converts it into a round.
lineToRound :: Int -> String -> Round
lineToRound 1 line = RoundPart1 (charToShape $ head line, charToShape $ line !! 2)
lineToRound 2 line = RoundPart2 (charToShape $ head line, charToOutcome $ line !! 2)

-- Converts an input character into a shape.
charToShape :: Char -> Shape
charToShape 'A' = Rock
charToShape 'X' = Rock
charToShape 'B' = Paper
charToShape 'Y' = Paper
charToShape 'C' = Scissors
charToShape 'Z' = Scissors

-- Converts an input character into an outcome.
charToOutcome :: Char -> Outcome
charToOutcome 'X' = Lose
charToOutcome 'Y' = Draw
charToOutcome 'Z' = Win

-- Converts a round into an outcome for part 1 of the problem.
roundToOutcome :: Round -> Outcome
roundToOutcome (RoundPart1 (Rock, Paper)) = Win
roundToOutcome (RoundPart1 (Rock, Scissors)) = Lose
roundToOutcome (RoundPart1 (Paper, Rock)) = Lose
roundToOutcome (RoundPart1 (Paper, Scissors)) = Win
roundToOutcome (RoundPart1 (Scissors, Rock)) = Win
roundToOutcome (RoundPart1 (Scissors, Paper)) = Lose
roundToOutcome (RoundPart1 (_, _)) = Draw

-- Converts a round into a shape for part 2 of the problem.
roundToShape :: Round -> Shape
roundToShape (RoundPart2 (Rock, Lose)) = Scissors
roundToShape (RoundPart2 (Rock, Win)) = Paper
roundToShape (RoundPart2 (Paper, Lose)) = Rock
roundToShape (RoundPart2 (Paper, Win)) = Scissors
roundToShape (RoundPart2 (Scissors, Lose)) = Paper
roundToShape (RoundPart2 (Scissors, Win)) = Rock
roundToShape (RoundPart2 (s, Draw)) = s

-- Calculates the points won from a given outcome.
outcomeToPoints :: Outcome -> Points
outcomeToPoints Lose = 0
outcomeToPoints Draw = 3
outcomeToPoints Win = 6

-- Calculates the points won from playing a given shape.
playToPoints :: Shape -> Points
playToPoints Rock = 1
playToPoints Paper = 2
playToPoints Scissors = 3

-- Calculates the points won from a given round.
getRoundPoints :: Round -> Points
getRoundPoints (RoundPart1 round) = outcomeToPoints (roundToOutcome (RoundPart1 round)) + playToPoints (snd round)
getRoundPoints (RoundPart2 round) = outcomeToPoints (snd round) + playToPoints (roundToShape (RoundPart2 round))
