module Helpers where

type Box = Char
type Stack = [Box]
type Move = (Int, Int, Int)
data CrateMover = CM9000 | CM9001 deriving (Eq)

rebuildStacks :: [Stack] -> Int -> (Int, Stack) -> (Int, Stack) -> [Stack]
rebuildStacks [] _ _ _ = []
rebuildStacks (x:xs) i (i1, s1) (i2, s2) = if i == i1
    then s1 : rebuildStacks xs (i + 1) (i1, s1) (i2, s2)
    else
        if i == i2
        then s2 : rebuildStacks xs (i + 1) (i1, s1) (i2, s2)
        else x : rebuildStacks xs (i + 1) (i1, s1) (i2, s2)

dropByIndex :: [x] -> Int -> [x]
dropByIndex x 0 = drop 1 x
dropByIndex x n = (take n x) ++ drop (n+1) x

executeMove :: [Stack] -> Move -> CrateMover -> [Stack]
executeMove stacks (0, _, _) _ = stacks
executeMove stacks (n, from, to) mover = executeMove newStacks (n - 1, from, to) mover
    where
        fromStack = stacks !! (from - 1)
        toStack = stacks !! (to - 1)
        i = if mover == CM9000
            then 0
            else n - 1
        box = fromStack !! i
        newFromStack = dropByIndex fromStack i
        newToStack = box : toStack
        newStacks = rebuildStacks stacks 0 (from - 1, newFromStack) (to - 1, newToStack)

executeMoves :: [Stack] -> [Move] -> CrateMover -> [Stack]
executeMoves stacks [] _ = stacks
executeMoves stacks (x:xs) mover = executeMoves (executeMove stacks x mover) xs mover

getTopsOfStacks :: [Stack] -> String
getTopsOfStacks [] = []
getTopsOfStacks (x:xs) = x !! 0 : getTopsOfStacks xs
