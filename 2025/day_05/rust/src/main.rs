mod database;
mod models;

use crate::database::{count_fresh_ingredients, parse_ingredients, unique_fresh_ingredients};
use std::fs;

fn main() {
    let lines = read_input_file();
    let (ranges, ingredients) = parse_ingredients(lines);

    let result_1 = count_fresh_ingredients(&ranges, &ingredients);
    println!("Part 1 result: {result_1}");

    let result_2 = unique_fresh_ingredients(&ranges);
    println!("Part 2 result: {result_2}");
}

fn read_input_file() -> Vec<String> {
    let input_file_path = "../input.txt";
    let mut input_file_lines: Vec<String> = Vec::new();
    for line in fs::read_to_string(input_file_path).unwrap().lines() {
        input_file_lines.push(line.to_string());
    }
    input_file_lines
}
