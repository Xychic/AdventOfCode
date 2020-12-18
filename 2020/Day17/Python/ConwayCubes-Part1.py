import sys
import collections
import itertools
import numpy as np

cells = collections.defaultdict(bool)
minX = minY = minZ = float("inf")
maxX = maxY = maxZ = 0

z = 0
for y, line in enumerate(open(f"{sys.path[0]}/../input.txt").read().splitlines()):
    for x, cell in enumerate(line):
        if cell == "#":
            cells[(x,y,z)] = True
            minX = min(minX, x)
            maxX = max(maxX, x)
            minY = min(minY, y)
            maxY = max(maxY, y)
            minZ = min(minZ, z)
            maxZ = max(maxZ, z)

def getNextCell(x, y, z):
    total = 0
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            for dz in range(-1, 2):
                if dx == dy == dz == 0:
                    continue
                if (cells[(x+dx, y+dy, z+dz)]):
                    total += 1
    if cells[(x, y, z)]:
        return total in [2,3]
    else:
        return total == 3            

for _ in range(6):
    nextGen = collections.defaultdict(bool)
    for x in range(minX-1, maxX+2):
        for y in range(minY-1, maxY+2):
            for z in range(minZ-1, maxZ+2):
                valid = getNextCell(x, y, z)
                nextGen[(x,y,z)] = valid
                if valid:
                    minX = min(minX, x)
                    maxX = max(maxX, x)
                    minY = min(minY, y)
                    maxY = max(maxY, y)
                    minZ = min(minZ, z)
                    maxZ = max(maxZ, z)
    cells = nextGen.copy()
    
print(sum([1 for c in cells.values() if c]))