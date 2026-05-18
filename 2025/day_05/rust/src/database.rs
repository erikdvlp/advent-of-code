use crate::models::{Ingredient, Range};

pub fn parse_ingredients(lines: Vec<String>) -> (Vec<Range>, Vec<Ingredient>) {
    let mut ranges: Vec<Range> = Vec::new();
    let mut ingredients: Vec<Ingredient> = Vec::new();

    let mut parsing_ranges = true;
    for line in lines {
        if line.is_empty() {
            parsing_ranges = false;
            continue;
        }

        if parsing_ranges {
            let parts: Vec<&str> = line.split('-').collect();
            let range = Range {
                lower: parts[0].parse().unwrap(),
                upper: parts[1].parse().unwrap(),
            };
            ranges.push(range);
        } else {
            let ingredient = Ingredient(line.parse().unwrap());
            ingredients.push(ingredient);
        }
    }

    merge_ranges(&mut ranges);

    (ranges, ingredients)
}

fn merge_ranges(ranges: &mut Vec<Range>) {
    ranges.sort();

    let mut a = 0;
    for b in 1..ranges.len() {
        if Range::overlaps(&ranges[a], &ranges[b]) {
            ranges[a].upper = ranges[a].upper.max(ranges[b].upper);
        } else {
            a += 1;
            ranges[a] = ranges[b].clone();
        }
    }

    ranges.truncate(a + 1);
}

pub fn count_fresh_ingredients(ranges: &[Range], ingredients: &[Ingredient]) -> u32 {
    let mut count = 0;
    for ingredient in ingredients {
        for range in ranges {
            if range.contains(ingredient) {
                count += 1;
                break;
            }
        }
    }
    count
}

pub fn unique_fresh_ingredients(ranges: &[Range]) -> usize {
    ranges.iter().map(|r| r.len()).sum()
}
