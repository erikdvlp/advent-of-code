result_1 = 0
result_2 = 0

with open('../../inputs/04.txt', 'r') as input_file:
    for line in input_file:
        l, r = line.strip().split(',')
        l1, l2 = map(int, l.split('-'))
        r1, r2 = map(int, r.split('-'))
        if (l1 <= r1 and l2 >= r2) or (l1 >= r1 and l2 <= r2): # full overlap
            result_1 += 1
            result_2 += 1
        elif (l1 <= r1 <= l2) or (l1 <= r2 <= l2): # partial overlap
            result_2 += 1

print(result_1)
print(result_2)
