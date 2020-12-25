import sys
import collections
import itertools
import numpy as np

def removeStrings(inp, strings):
    for s in strings:
        inp = inp.replace(s, "")
    return inp

bagWrapping = collections.defaultdict(list)

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    outer, inner = line.split(" bags contain ")
    inner = removeStrings(inner,["."," bags"," bag"]).split(", ")

    for i, bag in enumerate(inner):
        bags = bag.split(" ")
        num = bags[0]
        inner[i] = " ".join(bags[1:])
        bagWrapping[inner[i]].append(outer)

canContain = set()
toCheck = ["shiny gold"]
while len(toCheck):
    c = bagWrapping[toCheck.pop()]
    canContain.update(c)
    toCheck.extend(c)

print(len(canContain))