from math import ceil
from copy import deepcopy

useCount = {}
recipes = {}
for line in open("input.txt"):
    ingred, out = (line.strip().split(" => "))
    count, out = out.split()
    ingredients = []

    for a in ingred.split(", "):
        cnt, ing = a.split()
        if ing not in useCount:
            useCount[ing] = 0
        useCount[ing] += 1
        
        ingredients.append((int(cnt),ing))

    recipes[out] = (int(count),ingredients)


useCount["FUEL"] = 0
useCountCP = deepcopy(useCount)
lower = 1
upper = target = 1E12

while upper > lower:
    useCount = deepcopy(useCountCP)
    mid = int((upper + lower + 1)//2)
    totalNeeded = {"FUEL" : mid}

    while len(useCount) > 1:
        for ingredient in useCount:
            if useCount[ingredient] == 0:
                n = totalNeeded[ingredient]
                count, items = recipes[ingredient]
                amt = ceil(n/count)
                for itemAmt,item in items:
                    if item not in totalNeeded:
                        totalNeeded[item] = 0
                    totalNeeded[item] += amt*itemAmt
                    useCount[item] -=1
                del useCount[ingredient]
                break
            
    if (totalNeeded["ORE"]) <= target:
        lower = mid 
    else:
        upper = mid - 1
        
print(lower)