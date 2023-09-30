use assignment::*;
use std::fs;

mod assignment;

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
    let assignments: Vec<AssignmentPair> = input_file_lines
        .iter()
        .map(|l| line_to_assignment_pair(l))
        .collect();
    let full_overlaps: &Vec<Overlap> = &assignments
        .iter()
        .map(get_overlap)
        .filter(|o| matches!(o, Overlap::Full))
        .collect();
    let partial_overlaps: &Vec<Overlap> = &assignments
        .iter()
        .map(get_overlap)
        .filter(|o| matches!(o, Overlap::Partial | Overlap::Full))
        .collect();
    println!("Part 1 answer: {}", full_overlaps.len());
    println!("Part 2 answer: {}", partial_overlaps.len());
}
