grid = []
for line in open("input.txt"):
    grid.append(list(line.strip()))

totalKeys = set()
for y in range(len(grid)):
    for x in range(len(grid[y])):
        if grid[y][x] in [chr(i) for i in range(ord("a"), ord("z")+1)]:
            totalKeys.add(grid[y][x])
        elif grid[y][x] == "@":
            centre = (x, y)

x, y = centre
queue = [(x, y, set(), 0)]

previousStates = set()
while len(queue)>0:
    x, y, keys, dist = queue.pop(0)
    currentState = (x, y, tuple(keys))

    if currentState in previousStates:
        continue
    previousStates.add(currentState)
    if grid[y][x]=='#' or (grid[y][x] in [chr(i) for i in range(ord("A"),ord("Z")+1)] and grid[y][x].lower() not in keys):
        continue

    newkeys = keys.copy()
    if grid[y][x] in totalKeys:
        newkeys.add(grid[y][x])
        if newkeys == totalKeys:
            print(dist)
            break
    
    if y < len(grid)-1:
        queue.append((x, y+1, set(sorted(newkeys)), dist+1))
    if y > 0:
        queue.append((x, y-1, set(sorted(newkeys)), dist+1))
    if x < len(grid[0])-1: 
        queue.append((x+1, y, set(sorted(newkeys)), dist+1))
    if x > 0:
        queue.append((x-1, y, set(sorted(newkeys)), dist+1))

