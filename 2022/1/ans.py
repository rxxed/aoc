with open("input", 'r') as f:
    sum_ = 0
    sums = []
    for line in f.readlines():
        if line == '\n':
            sums.append(sum_)
            sum_ = 0
        else:
            sum_ += int(line.strip('\n'))
    print(sum(sorted(sums)[::-1][:3]))
