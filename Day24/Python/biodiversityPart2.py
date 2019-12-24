from copy import deepcopy
import numpy as np

grid = []
for line in open("../input.txt"):
    grid.append(list(line.strip()))

sizeX, sizeY = len(grid[0]), len(grid)
empty = [["." for _ in range(sizeX)] for _ in range(sizeY)]

levels = {0: grid}
maxL = minL = 0
nextLevel = {}

SHOW = False

def getVals(level, pos):
    x, y = pos
    neighbours = 0
    if x > 0:
        neighbours += 1 if levels[level][y][x-1] == "#" else 0
    if x < sizeX -1:
        neighbours += 1 if levels[level][y][x+1] == "#" else 0
    if y > 0:
        neighbours += 1 if levels[level][y-1][x] == "#" else 0
    if y < sizeY -1:
        neighbours += 1 if levels[level][y+1][x] == "#" else 0
    

    ## Checking up a level
    if x == 0:
        if level+1 in levels:
            neighbours += 1 if levels[level+1][2][1] == "#" else 0
    elif x == sizeX-1:
        if level+1 in levels:
            neighbours += 1 if levels[level+1][2][3] == "#" else 0
    if y == 0:
        if level+1 in levels:
            neighbours += 1 if levels[level+1][1][2] == "#" else 0
    elif y == sizeY-1:
        if level+1 in levels:
            neighbours += 1 if levels[level+1][3][2] == "#" else 0
    
    # Checking down a level
    if pos == (1,2):
        if level-1 in levels:
            for y in range(sizeY):
                neighbours += 1 if levels[level-1][y][0] == "#" else 0
    elif pos == (3,2):
        if level-1 in levels:
            for y in range(sizeY):
                neighbours += 1 if levels[level-1][y][sizeX-1] == "#" else 0
    elif pos == (2,1):
        if level-1 in levels:
            for x in range(sizeX):
                neighbours += 1 if levels[level-1][0][x] == "#" else 0

    elif pos == (2,3):
        if level-1 in levels:
            for x in range(sizeX):
                neighbours += 1 if levels[level-1][sizeY-1][x] == "#" else 0
    
    return neighbours

for min in range(200):
    levels[minL-1] = deepcopy(empty)
    levels[maxL+1] = deepcopy(empty)
    minL -= 1
    maxL += 1

    for i in range(minL, maxL+1):
        nextLevel[i] = [["." for x in range(sizeX)] for y in range(sizeY)]
        
        if SHOW:
            print(i)
            for g in levels[i]:
                print("".join(g))

        for y in range(sizeY):
            for x in range(sizeX):
                if (x,y) != (2,2):
                    neighbours = getVals(i, (x,y))
                    if levels[i][y][x] == "#" and neighbours == 1:
                        nextLevel[i][y][x] = "#"
                    elif levels[i][y][x] == "." and neighbours in [1,2]:
                        nextLevel[i][y][x] = "#"

    levels = nextLevel.copy()


print(sum([1 if l[y][x] == "#" else 0 for x in range(5) for y in range(5) for l in levels.values()]))
