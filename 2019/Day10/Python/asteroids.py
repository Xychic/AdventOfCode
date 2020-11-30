from math import atan2

asteroids = []
grid = []

for line in open("input.txt","r"):
    grid.append(list(line.strip()))

for y in range(len(grid)):
    for x in range(len(grid[y])):
        if grid[y][x] == "#":
            asteroids.append((x,y))


def getAngle(a, b):
    dx = b[0] - a[0]
    dy = a[1] - b[1]
    res = atan2(dx, dy)
    return res


result = float(0)
best = None

for current in asteroids:
    canSee = set()
    for ast in asteroids:
        if ast != current:
            dx = ast[0] - current[0]
            dy = ast[1] - current[1]
            canSee.add(getAngle(current, ast))
                
    if len(canSee) > result:
        best = current
        result = len(canSee)
    
print(best, result)