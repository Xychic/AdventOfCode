import sys
import collections
import itertools
import numpy as np

pos = [0, 0]
direction = 90

instructions = []
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    move = line[:1]
    amount = int(line[1:])
    instructions.append((move, amount))

for ins, amount in instructions:
    if ins == "F":
        ins = "NESW"[direction // 90]
    elif ins == "R":
        direction += amount
        direction %= 360
    elif ins == "L":
        direction -= amount
        direction %= 360
    if ins == "N":      pos[1] += amount
    elif ins == "E":    pos[0] += amount
    elif ins == "S":    pos[1] -= amount
    elif ins == "W":    pos[0] -= amount
        
print(abs(pos[0]) + abs(pos[1]))