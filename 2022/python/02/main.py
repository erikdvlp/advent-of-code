from helpers import part_1, part_2

with open('../../inputs/02.txt', 'r') as input_file:
    for line in input_file:
        round_outcome = tuple(line.strip().split(' '))
        for map in [part_1, part_2]:
            map.total_pts += map.round_outcome_pts[round_outcome] + map.play_pts[round_outcome[1]]

print(part_1.total_pts)
print(part_2.total_pts)
