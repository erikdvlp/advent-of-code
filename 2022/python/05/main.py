import copy

stacks = []
moves = []

def get_stacks_from_lines(stack_lines):
    def transpose(rows):
        return list(zip(*rows[::-1]))

    def remove_placeholders(columns):
        columns_list = list(map(list, columns))
        no_placeholders = []
        for column in columns_list:
            no_placeholders.append(list(filter(lambda x: x != None, column)))
        return no_placeholders

    rows = []
    for line in stack_lines:
        p1, p2 = 0, 3
        row = []
        while p2 < len(line):
            if line[p1:p2] == '   ':
                row.append(None)
            else:
                row.append(line[p1+1])
            p1, p2 = p1+4, p2+4
        rows.append(row)
    return remove_placeholders(transpose(rows))

def get_moves_from_lines(move_lines):
    moves = []
    for line in move_lines:
        line = line.split()
        moves.append(tuple(map(int, [line[1], line[3], line[5]])))
    return moves

def execute_moves(stacks, moves, from_top):
    for move in moves:
        num_boxes = move[0]
        from_stack = stacks[move[1]-1]
        to_stack = stacks[move[2]-1]
        index = len(from_stack)-num_boxes
        for _ in range(num_boxes):
            if from_top:
                index = len(from_stack)-1
            box = from_stack.pop(index)
            to_stack.append(box)
    return stacks

def get_tops_of_stacks(stacks):
    tops = ''
    for stack in stacks:
        tops += stack[len(stack)-1]
    return tops

with open('../../inputs/05.txt', 'r') as input_file:
    lines_to_process = []
    for line in input_file:
        if line.startswith(' 1'):
            stacks = get_stacks_from_lines(lines_to_process)
            lines_to_process = []
        lines_to_process.append(line)
    moves = get_moves_from_lines(lines_to_process[2:])

run = lambda from_top: get_tops_of_stacks(execute_moves(copy.deepcopy(stacks), moves, from_top))
result_1 = run(True)
result_2 = run(False)
print(result_1)
print(result_2)
