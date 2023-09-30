#[derive(Debug, Clone, PartialEq)]
pub enum FileType {
    File,
    Directory,
}
#[derive(Debug, Clone, PartialEq)]
pub struct FileNode {
    pub file_type: FileType,
    pub index: NodeIndex,
    pub file_name: String,
    pub file_size: usize,
    pub parent: Option<NodeIndex>,
    pub children: Option<Vec<NodeIndex>>,
}
pub type NodeIndex = usize;

/// Creates a new directory.
pub fn create_directory(index: NodeIndex, file_name: String) -> FileNode {
    FileNode {
        file_type: FileType::Directory,
        index: index,
        file_name: file_name,
        file_size: 0,
        parent: None,
        children: Some(Vec::new()),
    }
}

/// Creates a new file.
pub fn create_file(index: NodeIndex, file_name: String, file_size: usize) -> FileNode {
    FileNode {
        file_type: FileType::File,
        index: index,
        file_name: file_name,
        file_size: file_size,
        parent: None,
        children: None,
    }
}

/// Attaches a file node to another file node as a new child.
pub fn attach_child(a: &mut FileNode, b: &mut FileNode) {
    if let Some(children) = &mut a.children {
        children.push(b.index);
        b.parent = Some(a.index);
    }
}

/// Gets the index of the current file node's parent if it exists.
pub fn step_up(all_nodes: &Vec<FileNode>, curr_index: NodeIndex) -> Option<NodeIndex> {
    let curr_node = all_nodes.get(curr_index).unwrap();
    curr_node.parent
}

/// Gets the index of the current file node's child by name if it exists.
pub fn step_down(
    all_nodes: &Vec<FileNode>,
    curr_index: NodeIndex,
    target_name: String,
) -> Option<NodeIndex> {
    let curr_node = all_nodes.get(curr_index).unwrap();
    if let Some(children) = &curr_node.children {
        for child_index in children {
            let child = all_nodes.get(*child_index).unwrap();
            if child.file_name == target_name {
                return Some(*child_index);
            }
        }
    }
    None
}

/// Traverses a file node using DFS and calculates all directory sizes.
pub fn calc_dir_size(all_nodes: &mut Vec<FileNode>, curr_index: usize) -> usize {
    let node_clone = all_nodes.get(curr_index).unwrap().clone();
    let node_children = node_clone.children;
    let mut node_file_size = node_clone.file_size;
    if let None = node_children {
        return node_file_size;
    }
    if let Some(children) = node_children {
        for child_index in children {
            node_file_size += calc_dir_size(all_nodes, child_index);
        }
        let node = all_nodes.get_mut(curr_index).unwrap();
        node.file_size = node_file_size;
    }
    node_file_size
}

/// Iterates through all file nodes and sums its directory sizes that are <= 100K for part 1 of the problem.
pub fn sum_dirs_below_100k(all_nodes: &Vec<FileNode>) -> usize {
    let mut sum = 0;
    for node in all_nodes {
        if node.file_type == FileType::Directory && node.file_size < 100_000 {
            sum += node.file_size;
        }
    }
    sum
}

/// Iterates through all file nodes and gets the smallest directory whose deletion would free enough space for part 2 of the problem.
pub fn dir_to_delete_size(
    all_nodes: &Vec<FileNode>,
    space_needed: usize,
    root_dir_size: usize,
) -> usize {
    let mut minimum_size = root_dir_size;
    for node in all_nodes {
        if node.file_type == FileType::Directory
            && node.file_size >= space_needed
            && node.file_size < minimum_size
        {
            minimum_size = node.file_size;
        }
    }
    minimum_size
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generate_nodes() -> Vec<FileNode> {
        let mut all_nodes = Vec::new();
        let mut a = create_directory(0, String::from("a"));
        let mut b = create_directory(1, String::from("b"));
        let mut c = create_file(2, String::from("c"), 100);
        attach_child(&mut a, &mut b);
        attach_child(&mut b, &mut c);
        all_nodes.push(a);
        all_nodes.push(b);
        all_nodes.push(c);
        all_nodes
    }

    #[test]
    fn test_attach_child() {
        let all_nodes = generate_nodes();
        let expected_a = FileNode {
            file_type: FileType::Directory,
            index: 0,
            file_name: String::from("a"),
            file_size: 0,
            parent: None,
            children: Some(vec![1]),
        };
        let expected_b = FileNode {
            file_type: FileType::Directory,
            index: 1,
            file_name: String::from("b"),
            file_size: 0,
            parent: Some(0),
            children: Some(vec![2]),
        };
        let expected_c = FileNode {
            file_type: FileType::File,
            index: 2,
            file_name: String::from("c"),
            file_size: 100,
            parent: Some(1),
            children: None,
        };
        assert_eq!(*all_nodes.get(0).unwrap(), expected_a);
        assert_eq!(*all_nodes.get(1).unwrap(), expected_b);
        assert_eq!(*all_nodes.get(2).unwrap(), expected_c);
    }

    #[test]
    fn test_step_up() {
        let all_nodes = generate_nodes();
        assert_eq!(step_up(&all_nodes, 1), Some(0));
    }

    #[test]
    fn test_step_up_no_parent() {
        let all_nodes = generate_nodes();
        assert_eq!(step_up(&all_nodes, 0), None);
    }

    #[test]
    fn test_step_down() {
        let all_nodes = generate_nodes();
        assert_eq!(step_down(&all_nodes, 0, String::from("b")), Some(1));
    }

    #[test]
    fn test_step_down_no_child() {
        let all_nodes = generate_nodes();
        assert_eq!(step_down(&all_nodes, 0, String::from("invalid")), None);
    }

    #[test]
    fn test_calc_dir_size() {
        let mut all_nodes = generate_nodes();
        let root_file_size = calc_dir_size(&mut all_nodes, 0);
        assert_eq!(root_file_size, 100);
    }
}
