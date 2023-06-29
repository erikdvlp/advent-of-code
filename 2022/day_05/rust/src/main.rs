use parser::*;
use std::fs;
use supply_stacks::*;

mod parser;
mod supply_stacks;

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
    let stacks = lines_to_stacks(&input_file_lines);
    let moves = lines_to_moves(&input_file_lines);
    let stacks_1 = execute_moves(stacks.clone(), &moves, Mover::CM9000);
    let stacks_2 = execute_moves(stacks.clone(), &moves, Mover::CM9001);
    let result_1: Vec<Crate> = stacks_1.iter().map(|x| *x.first().unwrap()).collect();
    let result_2: Vec<Crate> = stacks_2.iter().map(|x| *x.first().unwrap()).collect();
    println!("Part 1 answer: {:?}", result_1);
    println!("Part 2 answer: {:?}", result_2);
}
