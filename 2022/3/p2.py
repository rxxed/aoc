from itertools import islice
import string

with open('input', 'r') as f:
    priority_sum = 0
    priority = dict(zip(string.ascii_letters, range(1, 53)))
    lines = iter(f.readlines())
    while group := list(islice(lines, 3)):
        common = ''.join(set(group[0].strip()).intersection(group[1].strip()))
        common = ''.join(set(group[2].strip()).intersection(common))
        priority_sum += priority[common]

print(priority_sum)
