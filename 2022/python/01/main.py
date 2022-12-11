elves = []

with open('../../inputs/01.txt','r') as input_file:
    elf = []
    for line in input_file:
        if line == '\n':
            elves.append(sum(elf))
            elf = []
        else:
            elf.append(int(line))

elves.sort(reverse=True)
result_1 = elves[0]
result_2 = sum(elves[:3])

print(result_1)
print(result_2)
