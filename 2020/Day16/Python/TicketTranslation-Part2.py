import sys
import collections
import itertools
import numpy as np

inp = open(f"{sys.path[0]}/../input.txt").read()

rules, mine, other = [line.split("\n") for line in inp.split("\n\n")]
rules = [r.split(": ")[1] for r in rules]
rules = [r.split(" or ") for r in rules]
other = [o.split(",") for o in other[1:-1]]
mine = mine[1].split(",")

for i, rule in enumerate(rules):
    newRule = []
    for j, subRule in enumerate(rule):
        a, b = subRule.split("-")
        newRule += [int(a), int(b)]
    rules[i] = newRule

validTickets = other.copy()
for o in other:
    for val in o:
        if not any(((a <= int(val) <= b) or (c <= int(val) <= d)) for a,b,c,d in rules):
            validTickets.remove(o)

validPos = [[True for _ in range(20)] for _ in range(20)]

for ticket in validTickets:
    for i, val in enumerate(ticket):
        for j, rule in enumerate(rules):
            a,b,c,d = rule
            if not ((a <= int(val) <= b) or (c <= int(val) <= d)):
                validPos[i][j] = False

mappingDict = {}
seen = []
while len(mappingDict) < 20:
    for i in range(20):
        possible = list(filter(lambda x : x not in seen and validPos[i][x], [j for j in range(20)]))
        if len(possible) == 1:
            mappingDict[possible[0]] = i
            seen.append(possible[0])

ans = 1
for i in range(6):
    ans *= int(mine[mappingDict[i]])
print(ans)