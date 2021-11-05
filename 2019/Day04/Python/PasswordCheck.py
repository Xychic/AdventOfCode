def checkNum(num):
    strNum = list(str(num))
    for i in range(len(strNum)-1):
        if strNum[i+1] < strNum[i]:
            return False

    for n in strNum:
        if strNum.count(n) >= 2:
            return True
    return False

total = 0
for i in range(125730,579381+1):
    if checkNum(i):
        total += 1
print(total)
 