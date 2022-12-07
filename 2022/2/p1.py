with open('input', 'r') as f:
    score = 0
    for line in f.readlines():
        op = line.split()[0]
        me = line.split()[2]

        dd = {
                'X'  :1, 'Y'  :2, 'Z'  :3,
                'A Y':6, 'B Z':6, 'C X':6,
                'A X':3, 'B Y':3, 'C Z':3
        }

        score += dd[me]
        if dd.get(line.strip()):
            score += dd[line.strip()]


print(score)
