use items::{lines_to_item_containers, sum_priority_for_items};
use std::fs;

mod items;

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
    let rucksacks = lines_to_item_containers(&input_file_lines, 1);
    let result_1 = sum_priority_for_items(rucksacks);
    let elf_groups = lines_to_item_containers(&input_file_lines, 2);
    let result_2 = sum_priority_for_items(elf_groups);
    println!("Part 1 answer: {}", result_1);
    println!("Part 2 answer: {}", result_2);
}
