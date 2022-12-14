misplaced_items = []
badges = []
result_1 = 0
result_2 = 0

def calc_priority(item):
    if 'a' <= item <= 'z':
        return ord(item)-ord('a') + 1
    else:
        return ord(item)-ord('A') + 27

def get_common_item(container):
    for item in container[0]:
        if item in container[1] and (len(container) == 2 or item in container[2]):
            return item

with open('../../inputs/03.txt', 'r') as input_file:
    elf_group = []
    for line in input_file:
        line = line.strip()
        elf_group.append(line)
        if len(elf_group) == 3:
            badges.append(get_common_item(elf_group))
            elf_group = []
        compartment_size = int(len(line) / 2)
        rucksacks = [line[:compartment_size], line[compartment_size:]]
        misplaced_items.append(get_common_item(rucksacks))

for item in misplaced_items:
    result_1 += calc_priority(item)
for badge in badges:
    result_2 += calc_priority(badge)

print(result_1)
print(result_2)
