with open('input', 'r') as f:
    count = 0
    for ln in f.readlines():
        elf1 = ln.split(',')[0].split('-')
        elf2 = ln.split(',')[1].split('-')

        r1 = range(int(elf1[0]), int(elf1[1])+1)
        r2 = range(int(elf2[0]), int(elf2[1])+1)

        if len(set(r1) & set(r2)) != 0:
            count += 1

print(count)
