import sys
import collections
import itertools
import numpy as np

acc = 0
instruction = 0
code = []
seen = []

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    op, arg = line.split()
    code.append((op, int(arg)))

while instruction not in seen:
    seen.append(instruction)
    op, arg = code[instruction]
    if op == "nop":
        instruction += 1
    elif op == "acc":
        acc += arg
        instruction += 1
    elif op == "jmp":
        instruction += arg
    
print(acc)
