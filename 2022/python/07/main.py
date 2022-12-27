from helpers import FileNode, traverse, add_child_to_curr_dir, change_dir, calc_dir_sizes

root = FileNode(
    name = '/',
    size = 0,
    fileType = 'directory',
    parent = None,
    children = []
)
result_1 = 0
result_2 = {
    'space_to_clear': 0,
    'candidates': [],
    'minimum': 0,
}

def calc_result_1(curr_node):
    if curr_node.size <= 100000 and curr_node.fileType == 'directory':
        global result_1
        result_1 += curr_node.size

def calc_result_2(curr_node):
    global result_2
    if curr_node.size >= result_2['space_to_clear'] and curr_node.fileType == 'directory':
        result_2['candidates'].append(curr_node.size)

with open('../../inputs/07.txt', 'r') as inputFile:
    curr_dir = root
    for line in inputFile:
        line = line.strip()
        if line.startswith('dir') or line[0].isdigit():
            add_child_to_curr_dir(line, curr_dir)
        elif line.startswith('$ cd') and not line == '$ cd /':
            curr_dir = change_dir(line, curr_dir)

traverse(root, calc_dir_sizes)
traverse(root, calc_result_1)
result_2['space_to_clear'] = 30000000 - (70000000 - root.size)
traverse(root, calc_result_2)
result_2['minimum'] = min(result_2['candidates'])

print(result_1)
print(result_2['minimum'])
