result_1 = 0
result_2 = 0

def is_unique(subsignal):
    chars_seen = ''
    for c in subsignal:
        if c in chars_seen:
            return False
        chars_seen += c
    return True

def find_marker(signal, n):
    p1, p2 = 0, n
    while p2 < len(signal):
        subsignal = signal[p1:p2]
        if is_unique(subsignal):
            return p2
        p1, p2 = p1+1, p2+1

with open('../../inputs/06.txt', 'r') as input_file:
    signal = input_file.read()

result_1 = find_marker(signal, 4)
result_2 = find_marker(signal, 14)
print(result_1)
print(result_2)
