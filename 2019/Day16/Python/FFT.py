def getPattern(pattern, index):
    res = []
    for x in pattern:
        res += [x] * index
    return res

with open("input.txt") as f:
    signal = [int(x) for x in list(f.read().strip())]

pattern = [0,1,0,-1]

def phase(signal, pattern):
    result = []
    for i in range(len(signal)):
        p = getPattern(pattern, i+1)
        total = 0
        for j in range(len(signal)):
            total += signal[j] * p[(j+1)%len(p)]
        result.append(abs(total)%10)
    return result

for i in range(100):
    signal = phase(signal,pattern)
    
for i in range(8):
    print(signal[i], end="")
print()