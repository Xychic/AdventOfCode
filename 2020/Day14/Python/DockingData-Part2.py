import sys
import collections
import itertools
import numpy as np

memory = {}
mask = ""

def fixFloating(mask, filled=""):
    if len(mask) == 0:
        return [int(filled, 2)]
    char = mask[0]
    if char == "X":
        return fixFloating(mask.replace("X","0",1), filled) + fixFloating(mask.replace("X","1",1), filled)
    else:
        return fixFloating(mask[1:], filled+char)


for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    op, data = line.split(" = ")
    if op == "mask":
        mask = data
    elif "mem" in op:
        addr = int(op[4:-1])
        addr = format(int(addr), 'b').zfill(36)
        bStr = ""
        for a, b in zip(mask, addr):
            if a == "X":
                bStr += "X"
            elif a == "0":
                bStr += b
            else:
                bStr += "1"
        for m in fixFloating(bStr):
            memory[m] = int(data)

print(sum(memory.values()))