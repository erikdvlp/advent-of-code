use std::collections::HashMap;
use std::fs;

/// Checks if a given part of a data stream is unique.
fn is_unique(data_part: &str) -> bool {
    let mut chars = HashMap::new();
    for c in data_part.chars() {
        if let Some(_) = chars.get(&c) {
            return false;
        }
        chars.insert(c, 0);
    }
    true
}

/// Finds the index of a packet marker in a given data stream.
fn find_marker(data: &String, size: usize) -> Option<usize> {
    let mut index = 0;
    while index + size < data.len() {
        let data_part = &data[index..index + size];
        if is_unique(data_part) {
            return Some(index + size);
        }
        index += 1;
    }
    None
}

/// Reads problem input from file.
fn read_input_file() -> String {
    let input_file_path = "../input.txt";
    fs::read_to_string(input_file_path).unwrap()
}

fn main() {
    let input_file_data = read_input_file();
    let result_1 = find_marker(&input_file_data, 4).unwrap();
    println!("Part 1 answer: {:?}", result_1);
    let result_2 = find_marker(&input_file_data, 14).unwrap();
    println!("Part 2 answer: {:?}", result_2);
}
