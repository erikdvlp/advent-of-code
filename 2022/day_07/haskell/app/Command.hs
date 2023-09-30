module Command where

import Data.List (find, isPrefixOf)
import Data.List.Split (splitOn)
import FileSystem

type Command = String

-- Parses a given input file line and runs the corresponding command.
processCommand :: (FileNode, Path) -> Command -> (FileNode, Path)
processCommand (f, p) command
    | "$ cd /" == command = (f, p)
    | "$ ls" == command = (f, p)
    | "$ cd" `isPrefixOf` command = (f, changeDirectory command (f, p))
    | "dir" `isPrefixOf` command = (addChildRecur f p $ createDirectory command, p)
    | otherwise = (addChildRecur f p $ createFile command, p)

-- Parses a given array of input file lines and runs each corresponding command.
processCommands :: (FileNode, Path) -> [Command] -> (FileNode, Path)
processCommands (f, p) [] = (f, p)
processCommands (f, p) (x : xs) = processCommands (processCommand (f, p) x) xs

-- Creates a file according to a given command.
createFile :: Command -> FileNode
createFile command = File name size
  where
    commandParts = splitOn [' '] command
    size = read (head commandParts)
    name = commandParts !! 1

-- Creates a directory according to a given command.
createDirectory :: Command -> FileNode
createDirectory command = Directory name 0 []
  where
    commandParts = splitOn [' '] command
    name = commandParts !! 1

-- Changes directory according to a given command.
changeDirectory :: Command -> (FileNode, Path) -> Path
changeDirectory "$ cd .." (f, p) = stepUp p
changeDirectory command (f, p) = stepDown p f name
  where
    commandParts = splitOn [' '] command
    name = commandParts !! 2
