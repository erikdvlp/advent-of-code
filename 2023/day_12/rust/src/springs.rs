use std::collections::HashMap;

type Springs = Vec<char>;
type Groups = Vec<usize>;
pub type Row = (Springs, Groups);
type Cache = HashMap<(usize, usize, usize), usize>;

/// Parses a given vector of input file lines and converts them into a vector of rows.
pub fn lines_to_rows(lines: &Vec<String>) -> Vec<Row> {
    let mut rows: Vec<Row> = Vec::new();
    for line in lines {
        let parts: Vec<&str> = line.split(' ').collect();
        let springs: Springs = parts.first().unwrap().chars().collect();
        let groups: Groups = parts[1]
            .split(',')
            .map(|x| x.parse::<usize>().unwrap())
            .collect();
        rows.push((springs, groups));
    }
    rows
}

/// Calculates the number of valid arrangements of a given row.
pub fn calc_arrangements(
    springs: &Springs,
    groups: &Groups,
    spring_index: usize,
    group_index: usize,
    damaged_count: usize,
    cache: &mut Cache,
) -> usize {
    let key = (spring_index, group_index, damaged_count);
    if cache.contains_key(&key) {
        return cache[&key];
    }
    if spring_index == springs.len() {
        if (group_index == groups.len() && damaged_count == 0)
            || (group_index == groups.len() - 1 && groups[group_index] == damaged_count)
        {
            return 1;
        } else {
            return 0;
        }
    }
    let mut arrangements = 0;
    for effective_spring in ['.', '#'] {
        if springs[spring_index] == '?' || springs[spring_index] == effective_spring {
            match effective_spring {
                '#' => {
                    arrangements += calc_arrangements(
                        springs,
                        groups,
                        spring_index + 1,
                        group_index,
                        damaged_count + 1,
                        cache,
                    );
                }
                _ => {
                    if damaged_count == 0 {
                        arrangements += calc_arrangements(
                            springs,
                            groups,
                            spring_index + 1,
                            group_index,
                            0,
                            cache,
                        );
                    } else if group_index < groups.len() && groups[group_index] == damaged_count {
                        arrangements += calc_arrangements(
                            springs,
                            groups,
                            spring_index + 1,
                            group_index + 1,
                            0,
                            cache,
                        );
                    }
                }
            }
        }
    }
    cache.insert(key, arrangements);
    arrangements
}

/// Unfolds a given row by repeating its springs and groups five times.
/// Only used for part 2 of the problem.
pub fn unfold_row(row: Row) -> Row {
    let (springs, groups) = row;
    let mut new_springs = Vec::new();
    let mut new_groups = Vec::new();
    for _ in 0..5 {
        new_springs.append(&mut springs.clone());
        new_springs.push('?');
        new_groups.append(&mut groups.clone());
    }
    new_springs.pop();
    (new_springs, new_groups)
}
