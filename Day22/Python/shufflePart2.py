def stack(size, a, b):
    return -a %size, (size-1-b)%size

def cut(amount, size, a, b):
        return a, (b-amount)%size
        
def deal(amount, size, a, b):
    return a*amount %size, b*amount %size

length = 119315717514047
repetitions = 101741582076661
pos = 2020
a, b = 1, 0

for line in open("../input.txt"):
    if "cut" in line:
        x = int(line.strip().split()[-1])
        a, b = cut(x, length, a, b)
    elif "increment" in line:
        x = int(line.strip().split()[-1])
        a, b = deal(x, length, a, b)
    elif "stack" in line:
        a, b = stack(length, a, b)

translation = (b * pow(1-a, length-2, length)) % length
res = ((pos - translation) * pow(a, repetitions*(length-2), length) + translation) % length
print(res)