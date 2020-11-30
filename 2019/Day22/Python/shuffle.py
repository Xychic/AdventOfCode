length = 10007
pos = 2019

for line in open("../input.txt"):
    if "cut" in line:
        x = int(line.strip().split()[-1])
        pos = (pos-x)%length
    elif "increment" in line:
        x = int(line.strip().split()[-1])
        pos = (pos * x)%length
    elif "stack" in line:
        pos = length - 1- pos

print(pos)