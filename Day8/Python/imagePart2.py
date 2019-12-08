image = []
for line in open("input.txt"):
    for c in line.strip():
        image.append(c)


layers = []
for i in range(0, len(image), 150):
    layers.append(image[i:i+150])


res = [[" " for i in range(25)] for j in range(6)]
for l in layers[::-1]:
    for j in range(6):
        for i in range(25):
            if l[j*25+i] == "0":
                res[j][i] = "_"
            elif l[j*25+i] == "1" :
                res[j][i] = "#"


for line in res:
    print("".join(line))
