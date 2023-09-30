type Calories = u32;
type Inventory = Vec<Calories>;

/// Converts input file lines into a vector of inventories.
pub fn lines_to_inventories(input_file_lines: Vec<String>) -> Vec<Inventory> {
    let mut inventories: Vec<Inventory> = Vec::new();
    let mut current_inventory: Inventory = Vec::new();
    for calories in input_file_lines {
        if !calories.is_empty() {
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
pub fn sum_inventories(inventories: Vec<Inventory>) -> Vec<Calories> {
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
pub fn sum_first_three_calories(calories: &[Calories]) -> Calories {
    calories[..=2].iter().sum()
}
