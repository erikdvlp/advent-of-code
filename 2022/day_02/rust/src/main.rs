use game::{lines_to_rounds, sum_points_for_rounds};
use std::fs;

mod game;

/// Reads problem input from file.
fn read_input_file() -> Vec<String> {
    let input_file_path = "../input.txt";
    let mut input_file_lines: Vec<String> = Vec::new();
    for line in fs::read_to_string(input_file_path).unwrap().lines() {
        input_file_lines.push(line.to_string());
    }
    input_file_lines
}

fn main() {
    let input_file_lines = read_input_file();
    let rounds_part_1 = lines_to_rounds(&input_file_lines, 1);
    let result_1 = sum_points_for_rounds(rounds_part_1);
    let rounds_part_2 = lines_to_rounds(&input_file_lines, 2);
    let result_2 = sum_points_for_rounds(rounds_part_2);
    println!("Part 1 answer: {}", result_1);
    println!("Part 2 answer: {}", result_2);
}
