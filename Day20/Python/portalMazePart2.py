grid = []
for line in open("input.txt"):
    grid.append(list(line.strip("\n")))

dy1 = [-2, 0, 1, 0]
dy2 = [-1, 0, 2, 0]
dx1 = [0, 1, 0, -2]
dx2 = [0, 2, 0, -1]
dy = [-1, 0, 1, 0]
dx = [0, 1, 0, -1]
portalDict =  {}

for y in range(2, len(grid)-2):
    for x in range(2, len(grid[y])-2):
        if grid[y][x] == ".":
            for i in range(4):
                a, b = grid[y+dy1[i]][x+dx1[i]], grid[y+dy2[i]][x+dx2[i]]
                if "A" <= a <= "Z" and "A" <= b <= "Z":
                    if a+b == "AA":
                        start = (x, y)
                    elif a+b == "ZZ":
                        end = (x, y)
                    if a+b not in portalDict:
                        portalDict[a+b] = []
                    portalDict[a+b].append((x, y))
                    portalDict[(x, y)] = a+b
                    break

queue = [(start, 0, 0)]  # pos, level, dist
seen = set([(start[0], start[1], 0)])  # pos x, pos y, level
while len(queue) > 0:
    pos, level, dist = queue.pop(0)
    if pos == end and level == 0:
        print(dist)
        break

    for i in range(4):
        x, y = pos[0] + dx[i], pos[1] + dy[i]
        nextLevel = level
        if "A" <= grid[y][x] <= "Z":
            if x == 1 or y == 1 or x == len(grid[0])-2 or y == len(grid)-2: 
                nextLevel -= 1
            else:        
                nextLevel += 1

            portal = portalDict[pos]            
            if portal not in ('AA', 'ZZ') and nextLevel >= 0:
                if pos == portalDict[portal][0]: 
                    x, y = portalDict[portal][1]
                else:                   
                    x, y = portalDict[portal][0]

        if ((x, y, nextLevel)) not in seen:
            seen.add((x, y, nextLevel))
            if grid[y][x] == '.':
                queue.append(((x, y), nextLevel, dist+1))