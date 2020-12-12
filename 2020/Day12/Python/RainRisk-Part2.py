import sys
import collections
import itertools
import numpy as np
import math

pos = [0, 0]
waypoint = [1, 10]

instructions = []
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    move = line[:1]
    amount = int(line[1:])
    instructions.append((move, amount))

for ins, amount in instructions:
    if ins == 'F':
        pos[0] += waypoint[0] * amount
        pos[1] += waypoint[1] * amount
    elif ins == "R":
        for _ in range(amount//90):
            x, y = waypoint
            waypoint = [-y, x]
    elif ins == "L":
        for _ in range(amount//90):
            x, y = waypoint
            waypoint = [y, -x]
    elif ins == 'N':    waypoint[0] += amount
    elif ins == 'E':    waypoint[1] += amount
    elif ins == 'S':    waypoint[0] -= amount
    elif ins == 'W':    waypoint[1] -= amount
       
print(abs(pos[0]) + abs(pos[1]))