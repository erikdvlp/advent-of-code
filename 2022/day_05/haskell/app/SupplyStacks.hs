module SupplyStacks where

type Crate = Char
type Stack = [Crate]
type Move = (Int, Int, Int)
data CrateMover = CM9000 | CM9001 deriving (Eq)

-- Rebuild the stacks with two stacks that have been modified.
rebuildStacks :: [Stack] -> Int -> (Int, Stack) -> (Int, Stack) -> [Stack]
rebuildStacks [] _ _ _ = []
rebuildStacks (x:xs) i (i1, s1) (i2, s2)
    | i == i1 = s1 : rebuildStacks xs (i + 1) (i1, s1) (i2, s2)
    | i == i2 = s2 : rebuildStacks xs (i + 1) (i1, s1) (i2, s2)
    | otherwise = x : rebuildStacks xs (i + 1) (i1, s1) (i2, s2)

-- Execute a single move on the stacks.
executeMove :: [Stack] -> Move -> CrateMover -> [Stack]
executeMove stacks (0, _, _) _ = stacks
executeMove stacks (n, from, to) mover = executeMove newStacks (n - 1, from, to) mover
    where
        fromStack = stacks !! (from - 1)
        toStack = stacks !! (to - 1)
        index = if mover == CM9000 then 0 else n - 1
        crate = fromStack !! index
        dropByIndex elems index = take index elems ++ drop (index+1) elems
        modFromStack = dropByIndex fromStack index
        modToStack = crate : toStack
        newStacks = rebuildStacks stacks 0 (from - 1, modFromStack) (to - 1, modToStack)

-- Recursively execute an array of moves on the stacks.
executeMoves :: [Stack] -> [Move] -> CrateMover -> [Stack]
executeMoves stacks [] _ = stacks
executeMoves stacks (x:xs) mover = executeMoves (executeMove stacks x mover) xs mover
