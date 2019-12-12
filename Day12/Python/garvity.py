def replaceAll(str, chars):
    for c in chars:
        str = str.replace(c,"")
    return str


def energy(moon):
    return sum([abs(x) for x in moon[0]])*sum([abs(x) for x in moon[1]])


moons = []
for line in open("input.txt","r"):
    moons.append([
        [int(x) for x in replaceAll(line, ["<",">","=","x","y","z","\n"]).split(", ")],
        [0,0,0]
    ])

for i in range(1000):
    for i in range(len(moons)):
        for j in range(i+1, len(moons)):
            a, b = moons[i], moons[j]

            for coord in range(3):
                if a[0][coord] < b[0][coord]:
                    a[1][coord] += 1
                    b[1][coord] -= 1
                elif a[0][coord] > b[0][coord]:
                    a[1][coord] -= 1
                    b[1][coord] += 1
            
    for m in moons:
        for i in range(3):
            m[0][i] += m[1][i]

print(sum([energy(m) for m in moons]))