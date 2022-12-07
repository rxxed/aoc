with open('input', 'r') as f:
    sc = 0
    for ln in f.readlines():
        n1 = ln.split()[0].strip()
        n2 = ln.split()[1].strip()

        dd = {
                'X':0, 'Y':3, 'Z':6,
                'A':3 if n2=='X' else 1 if n2=='Y' else 2,
                'B':1 if n2=='X' else 2 if n2=='Y' else 3,
                'C':2 if n2=='X' else 3 if n2=='Y' else 1
        }

        sc += dd[n1] + dd[n2]

print(sc)
