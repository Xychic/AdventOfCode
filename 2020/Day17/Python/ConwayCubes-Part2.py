import sys
import collections
import itertools
import numpy as np

cells = collections.defaultdict(bool)
minX = minY = minZ = minW = float("inf")
maxX = maxY = maxZ = maxW = 0

z = w = 0
for y, line in enumerate(open(f"{sys.path[0]}/../input.txt").read().splitlines()):
    for x, cell in enumerate(line):
        if cell == "#":
            cells[(x,y,z,w)] = True
            minX = min(minX, x)
            maxX = max(maxX, x)
            minY = min(minY, y)
            maxY = max(maxY, y)
            minZ = min(minZ, z)
            maxZ = max(maxZ, z)
            minW = min(minW, w)
            maxW = max(maxW, w)

def getNextCell(x, y, z, w):
    total = 0
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            for dz in range(-1, 2):
                for dw in range(-1, 2):
                    if dx == dy == dz == dw == 0:
                        continue
                    if (cells[(x+dx, y+dy, z+dz, w+dw)]):
                        total += 1
    if cells[(x, y, z, w)]:
        return total in [2,3]
    else:
        return total == 3            

for _ in range(6):
    nextGen = collections.defaultdict(bool)
    for x in range(minX-1, maxX+2):
        for y in range(minY-1, maxY+2):
            for z in range(minZ-1, maxZ+2):
                for w in range(minW-1, maxW+2):
                    valid = getNextCell(x, y, z,w)
                    nextGen[(x,y,z, w)] = valid
                    if valid:
                        minX = min(minX, x)
                        maxX = max(maxX, x)
                        minY = min(minY, y)
                        maxY = max(maxY, y)
                        minZ = min(minZ, z)
                        maxZ = max(maxZ, z)
                        minW = min(minW, w)
                        maxW = max(maxW, w)
    cells = nextGen.copy()
    
print(sum([1 for c in cells.values() if c]))