image = []
for line in open("input.txt"):
    for c in line.strip():
        image.append(int(c))


layers = []
for i in range(0, len(image), 150):
    layers.append(image[i:i+150])


minimum = float("INF")
result = None
for l in layers:
    if l.count(0) < minimum:
        minimum = l.count(0)
        result = l.count(1) * l.count(2)

print(result)
