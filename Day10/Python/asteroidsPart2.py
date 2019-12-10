from math import atan2, pi

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
    while res < 0:
        res += 2*pi
    return res


def getDist(a, b):
    return ((a[0] - b[0])**2 + (a[1] - b[1])**2)


centre = (20,18)

asteroids.remove(centre)
targets = []
for ast in asteroids:
    targets.append((ast, getDist(centre, ast), getAngle(centre, ast)))

targets.sort(key = lambda x: x[1])
targets.sort(key = lambda x: x[2])

count = 0
prevAng = -1
index = 0
while count < 200:
    index %= len(targets)

    ast, dist, ang = targets[index]
    if ang != prevAng:
        count += 1
        prevAng = ang
        result = ast
    index += 1

print(result)