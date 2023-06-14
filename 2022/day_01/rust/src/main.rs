use std::fs;

type Calories = u32;
type Inventory = Vec<Calories>;

/// Reads problem input from file.
fn read_input_file() -> Vec<String> {
    let input_file_path = "../input.txt";
    let mut input_file_lines: Vec<String> = Vec::new();
    for line in fs::read_to_string(input_file_path).unwrap().lines() {
        input_file_lines.push(line.to_string());
    }
    input_file_lines
}

/// Converts input file lines into a vector of inventories.
fn lines_to_inventories(input_file_lines: Vec<String>) -> Vec<Inventory> {
    let mut inventories: Vec<Inventory> = Vec::new();
    let mut current_inventory: Inventory = Vec::new();
    for calories in input_file_lines {
        if calories != "" {
            let calories: Calories = calories.parse().expect("Failed to parse line as integer");
            current_inventory.push(calories);
        } else {
            inventories.push(current_inventory);
            current_inventory = Vec::new();
        }
    }
    inventories.push(current_inventory);
    inventories
}

/// Calculates the caloric sum of each inventory given a vector of inventories and sorts in descending order.
fn sum_inventories(inventories: Vec<Inventory>) -> Vec<Calories> {
    let mut inventory_sums: Vec<Calories> = Vec::new();
    for inventory in inventories {
        let mut inventory_sum: Calories = 0;
        for calories in inventory {
            inventory_sum += calories;
        }
        inventory_sums.push(inventory_sum);
    }
    inventory_sums.sort_by(|a, b| b.cmp(a));
    inventory_sums
}

/// Sums the first three calories given a vector of calories.
fn sum_first_three_calories(calories: &Vec<Calories>) -> Calories {
    calories[..=2].iter().sum()
}

fn main() {
    let input_file_lines = read_input_file();
    let inventories = lines_to_inventories(input_file_lines);
    let inventory_sums = sum_inventories(inventories);
    let first_three_calories_sum = sum_first_three_calories(&inventory_sums);
    println!("Part 1 answer: {}", inventory_sums[0]);
    println!("Part 2 answer: {}", first_three_calories_sum);
}
