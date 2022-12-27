grid = []
result_1 = 0
result_2 = 0

with open('../../inputs/08.txt', 'r') as inputFile:
    for line in inputFile:
        line = line.strip()
        row = list(map(int, list(line)))
        grid.append(row)

grid_height = len(grid)
grid_width = len(grid[0])

def is_tree_visible(y, x):
    if y == 0 or x == 0 or y == grid_height-1 or x == grid_width-1:
        return True
    tree = grid[y][x]
    left = (tree > l for l in grid[y][:x])
    right = (tree > r for r in grid[y][x+1:])
    above = (tree > grid[a][x] for a in range(y - 1, -1, -1))
    below = (tree > grid[b][x] for b in range(y + 1, len(grid)))
    if all(left) or all(right) or all(above) or all(below):
        return True

def calc_scenic_score(y, x):
    def get_direction_score(tree, trees_in_view):
        score = 0
        for t in trees_in_view:
            score += 1
            if t >= tree:
                break
        return score
    tree = grid[y][x]
    left = get_direction_score(tree, list(reversed(grid[y][:x])))
    right = get_direction_score(tree, grid[y][x+1:])
    above = get_direction_score(tree, list(grid[a][x] for a in range(y - 1, -1, -1)))
    below = get_direction_score(tree, list(grid[b][x] for b in range(y + 1, len(grid))))
    return left * right * above * below

for y in range(grid_height):
    for x in range(grid_width):
        if is_tree_visible(y, x):
            result_1 += 1
        scenic_score = calc_scenic_score(y, x)
        result_2 = max(result_2, scenic_score)

print(result_1)
print(result_2)
