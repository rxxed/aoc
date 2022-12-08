with open('input', 'r') as f:
    count = 0
    for ln in f.readlines():
        elf1 = ln.split(',')[0].split('-')
        elf2 = ln.split(',')[1].split('-')

        l1 = [str(i) for i in range(int(elf1[0]), int(elf1[1])+1)]
        l2 = [str(i) for i in range(int(elf2[0]), int(elf2[1])+1)]

        if all(item in l1 for item in l2) or all(item in l2 for item in l1):
            count += 1

print(count)
