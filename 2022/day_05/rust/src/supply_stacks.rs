pub type Crate = char;
pub type Stack = Vec<Crate>;
pub type Move = (usize, usize, usize);

pub enum Mover {
    CM9000,
    CM9001,
}

// Execute moves on the stacks.
pub fn execute_moves(mut stacks: Vec<Stack>, moves: &Vec<Move>, mover: Mover) -> Vec<Stack> {
    for current_move in moves {
        let (n, from, to) = current_move;
        for index in 0..*n {
            let mut crate_index = 0;
            if let Mover::CM9001 = mover {
                crate_index = *n - index - 1;
            }
            let crate_to_move = stacks[*from].remove(crate_index);
            stacks[*to].insert(0, crate_to_move);
        }
    }
    stacks
}
