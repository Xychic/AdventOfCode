import sys
import collections
import itertools
import numpy as np

memory = {}
mask = ""

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    op, data = line.split(" = ")
    if op == "mask":
        mask = data
    else:
        addr = int(op[4:-1])
        data = format(int(data), 'b').zfill(36)
        bStr = ""
        for a, b in zip(mask, data):
            if a == "X":
                bStr += b
            else:
                bStr += a
        memory[addr] = int(bStr, 2)

print(sum(memory.values()))