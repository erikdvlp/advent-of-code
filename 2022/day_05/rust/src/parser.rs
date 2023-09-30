use crate::Move;
use crate::Stack;

// Transposes two-dimensional data.
fn transpose<T>(original: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let mut transposed = (0..original[0].len()).map(|_| vec![]).collect::<Vec<_>>();
    for original_row in original {
        for (item, transposed_row) in original_row.into_iter().zip(&mut transposed) {
            transposed_row.push(item);
        }
    }
    transposed
}

// Removes blank spaces from stacks of crates.
fn remove_spaces_from_stacks(stacks: Vec<Stack>) -> Vec<Stack> {
    let mut filtered_stacks: Vec<Stack> = Vec::new();
    for stack in stacks {
        let filtered_stack = stack
            .into_iter()
            .filter(|x| *x != ' ')
            .collect::<Vec<char>>();
        filtered_stacks.push(filtered_stack);
    }
    filtered_stacks
}

// Parses input file lines and returns stacks of crates.
pub fn lines_to_stacks(lines: &Vec<String>) -> Vec<Stack> {
    let mut rows: Vec<Vec<char>> = Vec::new();
    for line in lines {
        if &line[..2] == " 1" {
            break;
        }
        let mut row: Vec<char> = Vec::new();
        for index in (1..line.len()).step_by(4) {
            let crate_at_index = line.chars().nth(index).unwrap();
            row.push(crate_at_index);
        }
        rows.push(row);
    }
    let mut stacks: Vec<Stack> = transpose(rows);
    stacks = remove_spaces_from_stacks(stacks);
    stacks
}

// Parses input file lines and returns moves.
pub fn lines_to_moves(lines: &Vec<String>) -> Vec<Move> {
    let mut moves: Vec<Move> = Vec::new();
    for line in lines {
        if line.len() > 4 && &line[..4] == "move" {
            let words: Vec<&str> = line.split(' ').collect();
            let mut numbers: Vec<usize> = Vec::new();
            for index in (1..words.len()).step_by(2) {
                let number: usize = words.get(index).unwrap().parse().unwrap();
                numbers.push(number);
            }
            let current_move: Move = (
                *numbers.first().unwrap(),
                *numbers.get(1).unwrap() - 1,
                *numbers.get(2).unwrap() - 1,
            );
            moves.push(current_move);
        }
    }
    moves
}
