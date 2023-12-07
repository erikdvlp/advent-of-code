module Cards where

import Data.List (sort)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord)
data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind deriving (Eq, Ord)
data Hand = Hand {cards :: [Card], handType :: HandType}
data Play = Play {playId :: Int, hand :: Hand, bid :: Int, rank :: Int}
data ProblemPart = Part1 | Part2 deriving (Eq)

instance Eq Hand where
    (Hand _ handType1) == (Hand _ handType2) = handType1 == handType2

instance Ord Hand where
    (Hand cards1 handType1) `compare` (Hand cards2 handType2) =
        if handType1 == handType2
            then cards1 `compare` cards2
            else handType1 `compare` handType2

instance Eq Play where
    (Play _ hand1 _ _) == (Play _ hand2 _ _) = hand1 == hand2

instance Ord Play where
    (Play _ hand1 _ _) `compare` (Play _ hand2 _ _) = hand1 `compare` hand2

-- Converts a given character representing a card to a card.
charToCard :: ProblemPart -> Char -> Card
charToCard _ 'A' = Ace
charToCard _ 'K' = King
charToCard _ 'Q' = Queen
charToCard Part1 'J' = Jack
charToCard Part2 'J' = Joker
charToCard _ 'T' = Ten
charToCard _ '9' = Nine
charToCard _ '8' = Eight
charToCard _ '7' = Seven
charToCard _ '6' = Six
charToCard _ '5' = Five
charToCard _ '4' = Four
charToCard _ '3' = Three
charToCard _ _ = Two

-- Gets a hand type for a given list of cards.
getHandType :: [Card] -> HandType
getHandType cards
    | a == e = FiveKind
    | a == d || b == e = FourKind
    | (a == c && d == e) || (a == b && c == e) = FullHouse
    | a == c || b == d || c == e = ThreeKind
    | (a == b && c == d) || (b == c && d == e) || (a == b && d == e) = TwoPair
    | a == b || b == c || c == d || d == e = OnePair
    | otherwise = HighCard
  where
    [a, b, c, d, e] = sort cards

-- Replaces all jokers in a given list of cards with a given alternate card.
-- Only used for part 2 of the problem.
replaceJokers :: [Card] -> Card -> [Card]
replaceJokers [] _ = []
replaceJokers (x : xs) alt =
    if x == Joker
        then alt : replaceJokers xs alt
        else x : replaceJokers xs alt

-- Gets the best hand type that can be formed from a given list of cards that may contain jokers.
-- Only used for part 2 of the problem.
getBestHandType :: [Card] -> HandType
getBestHandType cards =
    if null altHandTypes
        then getHandType cards
        else maximum altHandTypes
  where
    alts = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Queen, King, Ace]
    altHandTypes = map (getHandType . replaceJokers cards) alts

-- Parses a given input file line and converts it to a play.
lineToPlay :: ProblemPart -> Int -> String -> Play
lineToPlay part id line = Play{playId = id, hand = hand, bid = bid, rank = 0}
  where
    parts = words line
    cards = map (charToCard part) $ head parts
    hand =
        if part == Part1
            then Hand{cards = cards, handType = getHandType cards}
            else Hand{cards = cards, handType = getBestHandType cards}
    bid = read $ last parts

-- Parses given input file lines and converts them into a list of plays.
linesToPlays :: ProblemPart -> Int -> [String] -> [Play]
linesToPlays _ _ [] = []
linesToPlays part i (x : xs) = lineToPlay part i x : linesToPlays part (i + 1) xs

-- Sets ranks for a given sorted list of plays.
setPlayRanks :: Int -> [Play] -> [Play]
setPlayRanks _ [] = []
setPlayRanks i (x : xs) = Play{playId = playId x, hand = hand x, bid = bid x, rank = i} : setPlayRanks (i + 1) xs

-- Gets the winnings from a given play.
getPlayWinnings :: Play -> Int
getPlayWinnings (Play _ _ bid rank) = bid * rank
