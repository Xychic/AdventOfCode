from copy import deepcopy
import numpy as np

grid = []
for line in open("../input.txt"):
    grid.append(list(line.strip()))

seen = set()

while True:
    nextGrid = [[None for x in range(len(grid[y]))] for y in range(len(grid))]

    for g in grid:
        print("".join(g))

    for y in range(len(grid)):
        for x in range(len(grid[y])):
            neighbours = 0
            if x > 0:
                neighbours += 1 if grid[y][x-1] == "#" else 0
            if x < len(grid[y]) -1:
                neighbours += 1 if grid[y][x+1] == "#" else 0
            if y > 0:
                neighbours += 1 if grid[y-1][x] == "#" else 0
            if y < len(grid) -1:
                neighbours += 1 if grid[y+1][x] == "#" else 0
            if grid[y][x] == "#" and neighbours == 1:
                nextGrid[y][x] = "#"
            elif grid[y][x] == "." and neighbours in [1,2]:
                nextGrid[y][x] = "#"
            else:
                nextGrid[y][x] = "."
    if tuple(np.array(grid).flatten()) in seen:
        res = np.array(grid).flatten()
        break
    else:
        seen.add(tuple(np.array(grid).flatten()))
    grid = deepcopy(nextGrid)
    print("")

total = 0
for i, b in enumerate(res):
    if b == "#":
        total += 2**i

print(total)