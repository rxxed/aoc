from itertools import islice


# generic parser to convert the stacks into python lists
def get_stacks(lines):
    length = int(lines[-1].strip()[-1])
    stacks = dict()
    for i in range(1, length+1):
        stacks[i] = list()

    for line in lines[:-1]:
        it = iter(line)
        n = 1
        while crate := list(islice(it, 4)):
            if crate[1] != ' ':
                stacks[n].append(crate[1])
            n += 1

    for i in range(1, length+1):
        stacks[i].reverse()

    return stacks, length


def main():
    fp = open('input', 'r')
    # get only the stacks portion of the input
    lines = fp.readlines()
    ni = lines.index('\n')
    first, second = lines[:ni], lines[ni+1:]
    stacks, length = get_stacks(first)

    for ln in second:
        lntokens = ln.split()
        num, popstack, pushstack = lntokens[1], lntokens[3], lntokens[5]
        for _ in range(int(num)):
            ele = stacks[int(popstack)].pop()
            stacks[int(pushstack)].append(ele)

    for i in range(1, length+1):
        print(stacks[i][-1], end='')
    print()


if __name__ == '__main__':
    main()
