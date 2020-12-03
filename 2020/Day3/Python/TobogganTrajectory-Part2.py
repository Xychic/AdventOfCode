import sys
import collections
import itertools
import numpy as np

world = []

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    world.append(list(line))

def findTree(a, b):
    x = y = total = 0

    while y < len(world):
        x = x % len(world[0])

        if world[y][x] == "#":
            total += 1
        
        x += a
        y += b
    return total


dirs = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
print(np.prod([findTree(a, b) for a, b in dirs]))