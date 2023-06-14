use std::collections::HashSet;

type Item = char;
type Priority = u64;

pub enum ItemContainer {
    Rucksack(String, String),
    ElfGroup(String, String, String),
}

/// Parses a given vector of input file lines and converts them into rucksacks or elf groups.
pub fn lines_to_item_containers(lines: &Vec<String>, problem_part: u8) -> Vec<ItemContainer> {
    let mut item_containers: Vec<ItemContainer> = Vec::new();
    match problem_part {
        1 => {
            for line in lines {
                let divider: usize = (line.len() + 1) / 2;
                let first: String = line[..divider].to_string();
                let second: String = line[divider..].to_string();
                let rucksack = ItemContainer::Rucksack(first, second);
                item_containers.push(rucksack);
            }
        }
        _ => {
            for index in (0..lines.len()).step_by(3) {
                let first: String = lines.get(index).unwrap().to_string();
                let second: String = lines.get(index + 1).unwrap().to_string();
                let third: String = lines.get(index + 2).unwrap().to_string();
                let elf_group = ItemContainer::ElfGroup(first, second, third);
                item_containers.push(elf_group);
            }
        }
    }
    item_containers
}

/// Gets a string representing the intersecting characters between two given strings.
fn intersect(first: String, second: String) -> String {
    let mut intersection: String = String::new();
    let mut first_contents: HashSet<char> = HashSet::new();
    for character in first.chars() {
        first_contents.insert(character);
    }
    for character in second.chars() {
        if first_contents.contains(&character) {
            intersection.push(character);
        }
    }
    intersection
}

/// Gets the first item in common in a rucksack or an elf group.
fn get_common_item(item_container: ItemContainer) -> Item {
    match item_container {
        ItemContainer::Rucksack(x, y) => return intersect(x, y).chars().next().unwrap(),
        ItemContainer::ElfGroup(x, y, z) => {
            return intersect(x, intersect(y, z)).chars().next().unwrap()
        }
    }
}

/// Calculates the priority of an item.
fn get_item_priority(item: Item) -> Priority {
    if item.is_ascii_lowercase() {
        return (item as u64) - ('a' as u64) + 1;
    }
    (item as u64) - ('A' as u64) + 27
}

/// Sums the total priorities from a given vector of item containers.
pub fn sum_priority_for_items(item_containers: Vec<ItemContainer>) -> Priority {
    let mut total_priority = 0;
    for item_container in item_containers {
        total_priority += get_item_priority(get_common_item(item_container));
    }
    total_priority
}
