grid = []
for line in open("input2.txt"):
    grid.append(list(line.strip("\n")))

dy1 = [-2, 0, 1, 0]
dy2 = [-1, 0, 2, 0]
dx1 = [0, 1, 0, -2]
dx2 = [0, 2, 0, -1]

portalDict = {}
start = end = None
for y in range(2, len(grid)-2):
    for x in range(2, len(grid[y])-2):
        if grid[y][x] == ".":
            for i in range(4):
                a, b = grid[y+dy1[i]][x+dx1[i]], grid[y+dy2[i]][x+dx2[i]]

                if "A" <= a <= "Z" and "A" <= b <= "Z":
                    # a, b = min(a,b), max(a,b)
                    if a+b == "AA":
                        start = (x, y)
                        break
                    elif a+b == "ZZ":
                        end = (x, y)
                        break
                    elif a+b not in portalDict:
                        portalDict[a+b] = []
                    portalDict[a+b].append((x, y))
                    break

teleports = {}
for k, v in portalDict.items():
    teleports[v[0]] = v[1]
    teleports[v[1]] = v[0]

print(start, end)
for k, v in portalDict.items():
    print(k, v)

x, y = start
grid[y][x] = 0
dy = [-1, 0, 1, 0]
dx = [0, 1, 0, -1]
res = set()
while True:
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if isinstance(grid[y][x], int):
                for i in range(4):
                    if isinstance(grid[y+dy[i]][x+dx[i]], int):
                        grid[y+dy[i]][x+dx[i]] = min(grid[y+dy[i]][x+dx[i]], grid[y][x]+1)
                    elif grid[y+dy[i]][x+dx[i]] == ".":
                        grid[y+dy[i]][x+dx[i]] = grid[y][x]+1
                    elif "A" <= grid[y+dy[i]][x+dx[i]]<= "Z" and (x,y) in teleports:
                        a, b = teleports[(x,y)]
                        if isinstance(grid[b][a], int):
                            grid[b][a] = min(grid[y][x]+1,grid[b][a])
                        elif grid[b][a] == ".":
                            grid[b][a] = grid[y][x]+1
    # for line in grid:
    #     for item in line:
    #         print(item, end="")
    #     print()
    a, b = end
    if grid[b][a] != ".":
        print(grid[b][a])
        break
