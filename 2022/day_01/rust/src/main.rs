use inventory::{lines_to_inventories, sum_first_three_calories, sum_inventories};
use std::fs;

mod inventory;

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
    let inventories = lines_to_inventories(input_file_lines);
    let inventory_sums = sum_inventories(inventories);
    let first_three_calories_sum = sum_first_three_calories(&inventory_sums);
    println!("Part 1 answer: {}", inventory_sums[0]);
    println!("Part 2 answer: {}", first_three_calories_sum);
}
