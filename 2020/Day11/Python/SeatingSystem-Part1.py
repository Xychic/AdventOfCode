import sys
import collections
import itertools
import numpy as np


def countNeigbours(world, x, y):
    if world[y, x] == ".":
        return "."
    lowerX = x if x == 0 else x-1
    upperX = x+1 if x == len(world[0])-1 else x+2
    lowerY = y if y == 0 else y-1
    upperY = y+1 if y == len(world)-1 else y+2
    seen = (world[lowerY:upperY, lowerX:upperX] == "#").sum() - (world[y][x] == "#")
    if world[y, x] == "L" and seen == 0:
        return "#"
    elif world[y, x] == "#" and seen >= 4:
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
