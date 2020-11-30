import heapq

grid = []
linearGrid = []
for line in open("input.txt"):
    grid.append(list(line.strip()))

alphabet = [chr(i) for i in range(ord("a"), ord("z")+1)]
ALPHABET = [chr(i) for i in range(ord("A"), ord("Z")+1)]
totalKeys = set()

for y in range(len(grid)):
    for x in range(len(grid[0])):
        if grid[y][x] in alphabet:
            totalKeys.add(grid[y][x])
        elif grid[y][x] == "@":
            grid[y][x-1] = "#"
            grid[y][x] = "#"
            grid[y][x+1] = "#"
            grid[y-1][x] = "#"
            grid[y+1][x] = "#"

            pos = (
                (x-1, y-1),
                (x+1, y-1),
                (x-1, y+1),
                (x+1, y+1),
            )


def visibleKeys(curX, curY, keys):
    queue = [(curX, curY, 0)]
    seen = set()
    dx = [0, 1, 0, -1]
    dy = [-1, 0, 1, 0]
    states = []
    while len(queue) > 0:
        x, y, dist = queue.pop(0)
        if grid[y][x] == "#" or (grid[y][x] in ALPHABET and not grid[y][x].lower() in keys):
            continue
        if (x, y) in seen:
            continue
        else:
            seen.add((x, y))
        if grid[y][x] in alphabet and grid[y][x] not in keys:
            states.append((dist, x, y, grid[y][x]))
            continue
        for i in range(4):
            queue.append((x + dx[i], y + dy[i], dist + 1))
    return states


queue = [(0, pos, frozenset())]
seen = [set(), set(), set(), set()]
while len(queue) > 0:
    dist, cpos, keys = heapq.heappop(queue)
    if keys == totalKeys:
        print(dist)
        break
    
    for i in range(len(cpos)):
        x, y = cpos[i]
        if (x, y, keys) in seen[i]:
            continue
        seen[i].add((x, y, keys))
        for extraDist, newX, newY, key in visibleKeys(x, y, keys):
            newPos = cpos[0:i] + ((newX, newY),) + cpos[i+1:]
            queue.append((dist + extraDist, newPos, keys | frozenset(key)))
