import itertools

with open ("input.txt","r") as file:
    code = [int(i) for i in file.read().split(",")]

def intComp(code, inputVal = [], start=0):
    outputVal = []
    i = start
    while True:
        i %= len(code)
        op = code[i] % 100
        ad1 = (code[i] // 100) % 10
        ad2 = (code[i] // 1000) % 10
        ad3 = (code[i] // 10000) % 10

        if op == 99:
            return None, val, i
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
            i += 2
            return code, p1, i
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


options = itertools.permutations([5,6,7,8,9])
result = float("-INF")
for order in options:
    val = 0
    startVal = [0 for _ in range(5)]
    inputs = [[order[i]] for i in range(5)]
    codes = [code.copy() for _ in range(5)]
    inputs[0].append(0)
    loop = True

    while loop:
        for i in range(5):
            newCode, val, newStart = intComp(codes[i], inputs[i], startVal[i])
            if newCode is None:
                result = max(result, val)
                loop = False
                break

            else:
                startVal[i] = newStart
                codes[i] = newCode
                inputs[(i+1)%5].append(val)
print(result)
