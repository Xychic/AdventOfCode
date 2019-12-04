def checkNum(num):
    strNum = list(str(num))
    for i in range(len(strNum)-1):
        if strNum[i+1] < strNum[i]:
            return False

    for n in strNum:
        for i in range(strNum.count(n),1,-1):
            if i == 2:
                return True
            elif n*i in "".join(strNum):
                break
    return False

total = 0
for i in range(125730,579382):
    if checkNum(i):
        total += 1
print(total)
 