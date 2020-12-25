import sys
import collections
import itertools
import numpy as np

def removeStrings(inp, strings):
    for s in strings:
        inp = inp.replace(s, "")
    return inp

def getTotalBags(bagName):
    total = 1    
    for count, bag in bagWrapping[bagName]:
        total += count * getTotalBags(bag)
    return total

bagWrapping = collections.defaultdict(list)

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    outer, inner = line.split(" bags contain ")

    if "no other" in inner:
        inner = []
    else:
        inner = removeStrings(inner,["."," bags"," bag"]).split(", ")

    for i, bag in enumerate(inner):
        bags = bag.split(" ")
        num = int(bags[0])
        inner[i] = (num, " ".join(bags[1:]))
    bagWrapping[outer] = inner

print(getTotalBags("shiny gold") - 1)