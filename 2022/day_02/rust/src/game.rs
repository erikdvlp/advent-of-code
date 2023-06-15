type Points = u32;

pub enum Shape {
    Paper,
    Rock,
    Scissors,
}
pub enum Outcome {
    Lose,
    Draw,
    Win,
}
pub enum Round {
    RoundPart1(Shape, Shape),
    RoundPart2(Shape, Outcome),
}

/// Parses a given vector of input file lines and converts them into a vector of rounds.
pub fn lines_to_rounds(lines: &Vec<String>, problem_part: u8) -> Vec<Round> {
    let mut rounds: Vec<Round> = Vec::new();
    for line in lines {
        let first = char_to_shape(&line[..1]);
        match problem_part {
            1 => {
                let second = char_to_shape(&line[2..]);
                let round = Round::RoundPart1(first, second);
                rounds.push(round);
            }
            _ => {
                let second = char_to_outcome(&line[2..]);
                let round = Round::RoundPart2(first, second);
                rounds.push(round);
            }
        }
    }
    rounds
}

/// Converts an input character into a shape.
fn char_to_shape(character: &str) -> Shape {
    match character {
        "A" => Shape::Rock,
        "X" => Shape::Rock,
        "B" => Shape::Paper,
        "Y" => Shape::Paper,
        &_ => Shape::Scissors,
    }
}

/// Converts an input character into an outcome.
fn char_to_outcome(character: &str) -> Outcome {
    match character {
        "X" => Outcome::Lose,
        "Y" => Outcome::Draw,
        &_ => Outcome::Win,
    }
}

/// Converts a round into an outcome for part 1 of the problem.
fn round_to_outcome(round: &Round) -> Outcome {
    match round {
        Round::RoundPart1(Shape::Rock, Shape::Paper) => Outcome::Win,
        Round::RoundPart1(Shape::Rock, Shape::Scissors) => Outcome::Lose,
        Round::RoundPart1(Shape::Paper, Shape::Scissors) => Outcome::Win,
        Round::RoundPart1(Shape::Paper, Shape::Rock) => Outcome::Lose,
        Round::RoundPart1(Shape::Scissors, Shape::Rock) => Outcome::Win,
        Round::RoundPart1(Shape::Scissors, Shape::Paper) => Outcome::Lose,
        _ => Outcome::Draw,
    }
}

/// Converts a round into a shape for part 2 of the problem.
fn round_to_shape(round: Round) -> Shape {
    match round {
        Round::RoundPart2(Shape::Rock, Outcome::Lose) => Shape::Scissors,
        Round::RoundPart2(Shape::Rock, Outcome::Win) => Shape::Paper,
        Round::RoundPart2(Shape::Paper, Outcome::Win) => Shape::Scissors,
        Round::RoundPart2(Shape::Scissors, Outcome::Lose) => Shape::Paper,
        Round::RoundPart2(s, Outcome::Draw) => s,
        _ => Shape::Rock,
    }
}

/// Calculates the points won from a given outcome.
pub fn outcome_to_points(outcome: &Outcome) -> Points {
    match outcome {
        Outcome::Lose => 0,
        Outcome::Draw => 3,
        Outcome::Win => 6,
    }
}

/// Calculates the points won from playing a given shape.
fn play_to_points(shape: &Shape) -> Points {
    match shape {
        Shape::Rock => 1,
        Shape::Paper => 2,
        Shape::Scissors => 3,
    }
}

/// Calculates the points won from a given round.
pub fn get_round_points(round: Round) -> Points {
    match round {
        Round::RoundPart1(_, ref y) => {
            outcome_to_points(&round_to_outcome(&round)) + play_to_points(y)
        }
        Round::RoundPart2(_, ref y) => {
            outcome_to_points(&y) + play_to_points(&round_to_shape(round))
        }
    }
}

/// Sums the total points won from a given vector of rounds.
pub fn sum_points_for_rounds(rounds: Vec<Round>) -> Points {
    let mut total_points: Points = 0;
    for round in rounds {
        total_points += get_round_points(round);
    }
    total_points
}
