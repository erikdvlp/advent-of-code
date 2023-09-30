use command::*;
use file_system::*;
use std::fs;

mod command;
mod file_system;

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
    let mut all_nodes = process_commands(input_file_lines);
    let root_dir_size = calc_dir_size(&mut all_nodes, 0);
    let result_1 = sum_dirs_below_100k(&all_nodes);
    println!("Part 1 answer: {:?}", result_1);
    let space_needed = 30000000 - (70000000 - root_dir_size);
    let result_2 = dir_to_delete_size(&all_nodes, space_needed, root_dir_size);
    println!("Part 2 answer: {:?}", result_2);
}
