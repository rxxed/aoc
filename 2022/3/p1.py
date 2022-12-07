import string

with open('input', 'r') as f:
    priority_sum = 0
    priority = dict(zip(string.ascii_letters, range(1, 53)))
    for ln in f.readlines():
        first, second = ln[:len(ln)//2], ln[len(ln)//2:]
        common = ''.join(set(first).intersection(second))
        priority_sum += priority[common]

print(priority_sum)
