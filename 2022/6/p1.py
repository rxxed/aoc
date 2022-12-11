from itertools import islice

with open('input', 'r') as f:
    stream = f.read()
    siglen = 4
    for i in range(len(stream) - siglen + 1):
        chunk = stream[i:i+siglen]
        if len(set(chunk)) == len(chunk):
            print(i+siglen)
            break
