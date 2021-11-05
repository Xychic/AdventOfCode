with open ("input.txt","r") as file:
    code = [int(i) for i in file.read().split(",")]

code[1] = 12
code[2] = 2

for i in range(0,len(code),4):
    if code[i] == 1:
        code[code[i+3]] = code[code[i+1]] + code[code[i+2]]
    elif code[i] == 2:
        code[code[i+3]] = code[code[i+1]] * code[code[i+2]]
    elif code[i] == 99:
        break   

print(code[0])