import sys
import collections
import itertools
import numpy as np


def countNeigbours(world, x, y):
    if world[y, x] == ".":
        return "."
    seen = 0
    for dy in range(-1, 2):
        for dx in range(-1, 2):
            if dx == dy == 0:
                continue
            multiply = 1
            while (x + dx*multiply not in [-1, len(world[0])]) and (y + dy*multiply not in [-1, len(world)]):
                if world[y + dy*multiply, x + dx*multiply] == "#":
                    seen += 1
                    break
                elif world[y + dy*multiply, x + dx*multiply] == "L":
                    break
                multiply += 1
    if world[y, x] == "L" and seen == 0:
        return "#"
    elif world[y, x] == "#" and seen >= 5:
        return "L"
    else:
        return world[y, x]
    
world = []

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    world.append(list(line))

world = np.array(world)
previous = -1
while previous != (world == "#").sum():
    previous = (world == "#").sum()
    print(previous)
    newWorld = np.array(world, copy=True)
    for y in range(len(world)):
        for x in range(len(world[y])):
            newWorld[y, x] = countNeigbours(world, x, y)
    world = np.array(newWorld)

print(previous)
