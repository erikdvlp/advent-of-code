use std::collections::HashMap;
use std::fs;

mod springs;

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
    let rows = springs::lines_to_rows(&input_file_lines);
    let result_1: usize = rows
        .iter()
        .map(|r| springs::calc_arrangements(&(r.0), &(r.1), 0, 0, 0, &mut HashMap::new()))
        .sum();
    println!("Part 1 answer: {result_1}");
    let unfolded_rows: Vec<springs::Row> = rows
        .iter()
        .map(|r| springs::unfold_row(r.clone()))
        .collect();
    let result_2: usize = unfolded_rows
        .iter()
        .map(|r| springs::calc_arrangements(&(r.0), &(r.1), 0, 0, 0, &mut HashMap::new()))
        .sum();
    println!("Part 2 answer: {result_2}");
}
