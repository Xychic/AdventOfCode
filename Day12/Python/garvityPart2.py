def replaceAll(str, chars):
    for c in chars:
        str = str.replace(c,"")
    return str
    

def GCD(a, b):
    return a if b == 0 else GCD(b, a%b)


def LCM(a, b):
    return (a*b)//GCD(a,b)


def LCMList(a):
    if len(a) == 1:
        return a[0]
    else:
        a.append(LCM(a.pop(), a.pop()))
        return LCMList(a)


coords = [[], [], []]
periods = [0, 0, 0]
moons = []
for line in open("input.txt","r"):
    moons.append([
        [int(x) for x in replaceAll(line, ["<",">","=","x","y","z","\n"]).split(", ")],
        [0,0,0]
    ])

for m in moons:
    for i in range(3):
        coords[i].append([m[0][i], m[1][i]])

for k in range(len(coords)):
    c = coords[k]
    previousStates = set()

    while True:
        for i in range(len(c)):
            for j in range(i+1, len(c)):
                a, b = c[i], c[j]

                if a[0] < b[0]:
                    a[1] += 1
                    b[1] -= 1
                elif a[0] > b[0]:
                    a[1] -= 1
                    b[1] += 1

        current = []
        for val in c:
            val[0] += val[1]
            current.append(str(val[0]))
            current.append(str(val[1]))
        current = ",".join(current)

        if current in previousStates:
            break
        else:
            previousStates.add(current)
    periods[k] = len(previousStates)

print(LCMList(periods))