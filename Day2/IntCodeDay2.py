with open ("input.txt","r") as file:
    code = [int(i) for i in file.read().split(",")]

for k in range(len(code)):
    for j in range(len(code)):
        res = code.copy()
        res[1] = k
        res[2] = j
        for i in range(0,len(res),4):
            # print(i,res[i:i+4])
            if res[i] == 1:
                res[res[i+3]] = res[res[i+1]] + res[res[i+2]]
            elif res[i] == 2:
                res[res[i+3]] = res[res[i+1]] * res[res[i+2]]
            elif res[i] == 99:
                break         

        if res[0] == 19690720:
            print(k,j)
            input()