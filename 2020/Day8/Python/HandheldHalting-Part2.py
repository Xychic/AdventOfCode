import sys
import collections
import itertools
import numpy as np

def run(code):
    acc = 0
    instruction = 0
    seen = []
    while instruction not in seen:
        seen.append(instruction)
        
        if instruction >= len(code):
            return True, acc
        op, arg = code[instruction]
        if op == "nop":
            instruction += 1
        elif op == "acc":
            acc += arg
            instruction += 1
        elif op == "jmp":
            instruction += arg
    return False, acc

code = []

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    op, arg = line.split()
    code.append((op, int(arg)))

instruction = 0
while True:
    op, arg = code[instruction]
    x = code.copy()

    if op == "acc":
        instruction += 1

    elif op == "nop":
        x[instruction] = ("jmp", arg)
        valid, ans = run(x)
        instruction += 1

    elif op == "jmp":
        x[instruction] = ("nop", arg)
        instruction += arg

    valid, ans = run(x)
    if valid:
        print(ans)
        break