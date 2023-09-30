use crate::attach_child;
use crate::calc_dir_size;
use crate::create_directory;
use crate::create_file;
use crate::dir_to_delete_size;
use crate::step_down;
use crate::step_up;
use crate::sum_dirs_below_100k;
use crate::FileNode;
use crate::FileType;
use crate::NodeIndex;

/// Parses a given input file line and runs the corresponding command.
fn process_command(
    all_nodes: &mut Vec<FileNode>,
    curr_index: NodeIndex,
    command: String,
) -> NodeIndex {
    if command != "$ cd /" && command != "$ ls" {
        if &command[..3] == "dir" {
            process_create_directory(all_nodes, curr_index, command);
        } else if &command[..4] == "$ cd" {
            return process_change_directory(all_nodes, curr_index, command);
        } else {
            process_create_file(all_nodes, curr_index, command);
        }
    }
    curr_index
}

/// Parses a given array of input file lines and runs each corresponding command.
pub fn process_commands(commands: Vec<String>) -> Vec<FileNode> {
    let mut all_nodes = Vec::new();
    let root = FileNode {
        file_type: FileType::Directory,
        index: 0,
        file_name: String::from("/"),
        file_size: 0,
        parent: None,
        children: Some(Vec::new()),
    };
    all_nodes.push(root);
    let mut curr_index = 0;
    for command in commands {
        curr_index = process_command(&mut all_nodes, curr_index, command);
    }
    all_nodes
}

/// Creates a directory according to a given command.
fn process_create_directory(all_nodes: &mut Vec<FileNode>, curr_index: NodeIndex, command: String) {
    let command_parts: Vec<&str> = command.split(' ').collect();
    let file_name = command_parts.get(1).unwrap().to_string();
    let mut new_dir = create_directory(all_nodes.len(), file_name);
    let mut curr_node = all_nodes.get_mut(curr_index).unwrap();
    attach_child(&mut curr_node, &mut new_dir);
    all_nodes.push(new_dir);
}

/// Creates a file according to a given command.
fn process_create_file(all_nodes: &mut Vec<FileNode>, curr_index: NodeIndex, command: String) {
    let command_parts: Vec<&str> = command.split(' ').collect();
    let file_name = command_parts.get(1).unwrap().to_string();
    let file_size = command_parts.first().unwrap().parse().unwrap();
    let mut new_file = create_file(all_nodes.len(), file_name, file_size);
    let mut curr_node = all_nodes.get_mut(curr_index).unwrap();
    attach_child(&mut curr_node, &mut new_file);
    all_nodes.push(new_file);
}

/// Changes directory according to a given command.
fn process_change_directory(
    all_nodes: &mut Vec<FileNode>,
    curr_index: NodeIndex,
    command: String,
) -> NodeIndex {
    let new_index: Option<NodeIndex>;
    let command_parts: Vec<&str> = command.split(' ').collect();
    let target_name = command_parts.get(2).unwrap().to_string();
    if target_name == ".." {
        new_index = step_up(all_nodes, curr_index);
    } else {
        new_index = step_down(all_nodes, curr_index, target_name);
    }
    if let Some(index) = new_index {
        return index;
    }
    curr_index
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_commands() {
        let commands = vec![
            String::from("$ cd /"),
            String::from("$ ls"),
            String::from("dir a"),
            String::from("14848514 b.txt"),
            String::from("8504156 c.dat"),
            String::from("dir d"),
            String::from("$ cd a"),
            String::from("$ ls"),
            String::from("dir e"),
            String::from("29116 f"),
            String::from("2557 g"),
            String::from("62596 h.lst"),
            String::from("$ cd e"),
            String::from("$ ls"),
            String::from("584 i"),
            String::from("$ cd .."),
            String::from("$ cd .."),
            String::from("$ cd d"),
            String::from("$ ls"),
            String::from("4060174 j"),
            String::from("8033020 d.log"),
            String::from("5626152 d.ext"),
            String::from("7214296 k"),
        ];
        let mut all_nodes = process_commands(commands);
        assert_eq!(all_nodes.len(), 14);
        let root_dir_size = calc_dir_size(&mut all_nodes, 0);
        assert_eq!(root_dir_size, 48381165);
        let sum = sum_dirs_below_100k(&all_nodes);
        assert_eq!(sum, 95437);
        let space_needed = 30000000 - (70000000 - root_dir_size);
        let size = dir_to_delete_size(&all_nodes, space_needed, root_dir_size);
        assert_eq!(size, 24933642);
    }
}
