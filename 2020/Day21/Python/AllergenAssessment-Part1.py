import sys
import collections
import itertools
import numpy as np

inp = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"""
inp = open(f"{sys.path[0]}/../input.txt").read()

allIngred = set()
allCount = collections.defaultdict(int)
possible = collections.defaultdict(list)

for line in inp.splitlines():
    ingredients, allergens = line.split("(contains ")
    ingredients = ingredients.split()
    allergens = allergens[:-1].split(", ")
    for a in allergens:
        possible[a].append(set(ingredients))
    for i in ingredients:
        allIngred.add(i)
        allCount[i] += 1
for k, v in possible.items():
    res = v[0]
    for item in v:
        res &= item
    allIngred -= res

print(sum([allCount[i] for i in allIngred]))

