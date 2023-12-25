mod heat;

fn main() {
    let grid = heat::lines_to_grid();
    let result_1 = heat::pathfind(
        &grid,
        (0, 0, heat::Direction::East, 0),
        heat::ProblemPart::Part1,
    );
    println!("Part 1 answer: {result_1}");
    let result_2 = heat::pathfind(
        &grid,
        (0, 0, heat::Direction::East, 0),
        heat::ProblemPart::Part2,
    );
    println!("Part 2 answer: {result_2}");
}
