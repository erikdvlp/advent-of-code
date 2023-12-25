use priority_queue::DoublePriorityQueue;
use std::collections::HashMap;
use std::fs;

type HeatLoss = usize;
type Coord = usize;
type Priority = usize;
type Steps = usize;
type Node = (Coord, Coord, Direction, Steps);
type Grid = Vec<Vec<HeatLoss>>;

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum Direction {
    North,
    East,
    South,
    West,
}
pub enum ProblemPart {
    Part1,
    Part2,
}

/// Reads problem input from file and converts it into a grid.
pub fn lines_to_grid() -> Grid {
    let input_file_path = "../input.txt";
    let mut input_file_lines: Vec<String> = Vec::new();
    for line in fs::read_to_string(input_file_path).unwrap().lines() {
        input_file_lines.push(line.to_string());
    }
    let mut rows: Grid = Vec::new();
    for line in input_file_lines {
        let row: Vec<usize> = line
            .chars()
            .map(|x| x.to_digit(10).unwrap() as usize)
            .collect();
        rows.push(row);
    }
    rows
}

/// Gets all valid neighbours for a given node.
fn get_neighbours(grid: &Grid, node: &Node, part: &ProblemPart) -> Vec<Node> {
    let (x, y, dir, steps) = node;
    let mut neighbours: Vec<Node> = Vec::new();
    if let Some(val) = y.checked_sub(1) {
        if *dir != Direction::South {
            neighbours.push((
                *x,
                val,
                Direction::North,
                if *dir == Direction::North {
                    steps + 1
                } else {
                    1
                },
            ));
        }
    }
    if let Some(val) = x.checked_sub(1) {
        if *dir != Direction::East {
            neighbours.push((
                val,
                *y,
                Direction::West,
                if *dir == Direction::West {
                    steps + 1
                } else {
                    1
                },
            ));
        }
    }
    if x + 1 < grid[0].len() && *dir != Direction::West {
        neighbours.push((
            *x + 1,
            *y,
            Direction::East,
            if *dir == Direction::East {
                steps + 1
            } else {
                1
            },
        ));
    }
    if y + 1 < grid.len() && *dir != Direction::North {
        neighbours.push((
            *x,
            *y + 1,
            Direction::South,
            if *dir == Direction::South {
                steps + 1
            } else {
                1
            },
        ));
    }
    match part {
        ProblemPart::Part1 => neighbours
            .into_iter()
            .filter(|(_, _, _, n_steps)| n_steps <= &3)
            .collect(),
        ProblemPart::Part2 => neighbours
            .into_iter()
            .filter(|(_, _, n_dir, n_steps)| {
                if steps < &4 {
                    n_dir == dir
                } else {
                    n_steps <= &10
                }
            })
            .collect(),
    }
}

/// Finds the least possible heat loss through a given grid using a modified Dijkstra's algorithm.
pub fn pathfind(grid: &Grid, start: Node, part: ProblemPart) -> usize {
    let mut queue: DoublePriorityQueue<Node, Priority> = DoublePriorityQueue::new();
    let mut heat_loss: HashMap<Node, HeatLoss> = HashMap::new();
    queue.push(start, 0);
    heat_loss.insert(start, 0);

    while !queue.is_empty() {
        let current = queue.pop_min().unwrap().0;
        for neighbour in get_neighbours(grid, &current, &part) {
            let neighbour_heat_loss = heat_loss[&current] + grid[neighbour.1][neighbour.0];
            if !heat_loss.contains_key(&neighbour) || heat_loss[&neighbour] > neighbour_heat_loss {
                queue.push(neighbour, neighbour_heat_loss);
                heat_loss.insert(neighbour, neighbour_heat_loss);
            }
        }
    }

    let mut min_heat_loss = usize::MAX;
    for ((x, y, _, s), h) in heat_loss {
        if x == grid[0].len() - 1 && y == grid.len() - 1 {
            match part {
                ProblemPart::Part1 => {
                    if h < min_heat_loss {
                        min_heat_loss = h;
                    }
                }
                ProblemPart::Part2 => {
                    if h < min_heat_loss && s >= 4 {
                        min_heat_loss = h;
                    }
                }
            }
        }
    }
    min_heat_loss
}
