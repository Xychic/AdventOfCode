import itertools

with open ("input.txt","r") as file:
    code = [int(i) for i in file.read().split(",")]

def intComp(code, inputVal = []):
    outputVal = []
    i = 0
    while True:
        i %= len(code)
        op = code[i] % 100
        ad1 = (code[i] // 100) % 10
        ad2 = (code[i] // 1000) % 10
        ad3 = (code[i] // 10000) % 10

        if op == 99:
            return outputVal
        elif op == 1:
            p1 = code[i+1] if ad1==1 else code[code[i+1]]
            p2 = code[i+2] if ad2==1 else code[code[i+2]]
            code[code[i+3]] = p1 + p2
            i += 4
        elif op == 2:
            p1 = code[i+1] if ad1==1 else code[code[i+1]]
            p2 = code[i+2] if ad2==1 else code[code[i+2]]   
            code[code[i+3]] = p1 * p2
            i += 4
        elif op == 3:
            code[code[i+1]] = inputVal.pop(0)
            i += 2
        elif op == 4:
            p1 = code[i+1] if ad1==1 else code[code[i+1]]
            outputVal.append(p1)
            i += 2
        elif op == 5:
            p1 = code[i+1] if ad1==1 else code[code[i+1]]
            p2 = code[i+2] if ad2==1 else code[code[i+2]]
            i = p2 if p1 == 1 else i+3
        elif op == 6:
            p1 = code[i+1] if ad1==1 else code[code[i+1]]
            p2 = code[i+2] if ad2==1 else code[code[i+2]]
            i = p2 if p1 == 0 else i+3
        elif op == 7:
            p1 = code[i+1] if ad1==1 else code[code[i+1]]
            p2 = code[i+2] if ad2==1 else code[code[i+2]]
            code[code[i+3]] = 1 if p1<p2 else 0
            i += 4
        elif op == 8:
            p1 = code[i+1] if ad1==1 else code[code[i+1]]
            p2 = code[i+2] if ad2==1 else code[code[i+2]]
            code[code[i+3]] = 1 if p1==p2 else 0
            i += 4

options = itertools.permutations([0,1,2,3,4])
# code = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
# 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
result = 0
for order in options:
    val = 0
    for o in order:
        val = intComp(code,[o,val])[0]
    result = max(result, val)

print(result)