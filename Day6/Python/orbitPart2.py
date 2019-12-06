orbits = {}

with open("input.txt", "r") as file:
    for line in file:
        orb = line.replace("\n","").split(")")
        x = orb[0]
        y = orb[1]
        if x not in orbits:
            orbits[x] = []
        orbits[x].append(y)
        if y not in orbits:
            orbits[y] = []
        orbits[y].append(x)

visited = {}
toVisit = [("SAN",0)]

while len(toVisit) > 0:
    pos, dist = toVisit.pop(-1)
    if pos not in visited:
        visited[pos] = dist
        for y in orbits[pos]:
            toVisit.append((y,dist+1))

print(visited["YOU"] -2)
