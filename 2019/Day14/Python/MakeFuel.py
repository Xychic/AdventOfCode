from math import ceil

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
totalNeeded = {"FUEL" : 1}

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

print(totalNeeded["ORE"])