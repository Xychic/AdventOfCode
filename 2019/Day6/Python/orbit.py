orbits = {}

with open("input.txt", "r") as file:
    for line in file:
        orb = line.replace("\n","").split(")")
        x = orb[0]
        y = orb[1]
        if x not in orbits:
            orbits[x] = []
        orbits[x].append(y)


def sumAll(x):
    res = 0
    if x in orbits:
        for y in orbits[x]:
            res += sumAll(y)
            res += 1
        return res
    else:
        return 0

res = 0
for orb in orbits:
    res += sumAll(orb)

print(res)
