class FileNode:
    def __init__(self, name, size, fileType, parent, children):
        self.name = name
        self.size = size
        self.fileType = fileType
        self.parent = parent
        self.children = children

def traverse(curr_node, f):
    if curr_node.children:
        for child in curr_node.children:
            traverse(child, f)
    f(curr_node)

def add_child_to_curr_dir(line, curr_dir):
    is_dir = line.startswith('dir')
    line_parts = line.split()
    new_child = FileNode(
        name = line_parts[1],
        size = 0 if is_dir else int(line_parts[0]),
        fileType = 'directory' if is_dir else 'file',
        parent = curr_dir,
        children = []
    )
    curr_dir.children.append(new_child)

def change_dir(line, curr_dir):
    new_dir_name = line.split()[2]
    if new_dir_name == '..':
        return curr_dir.parent
    for child in curr_dir.children:
        if child.name == new_dir_name and child.fileType == 'directory':
            return child

def calc_dir_sizes(curr_node):
    if curr_node.parent:
        curr_node.parent.size += curr_node.size
