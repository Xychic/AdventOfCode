import sys
import collections
import itertools
import numpy as np

world = []
x = y = total = 0

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    world.append(list(line))

while y < len(world):
    x = x % len(world[0])

    if world[y][x] == "#":
        total += 1
    
    x += 3
    y += 1

print(total)