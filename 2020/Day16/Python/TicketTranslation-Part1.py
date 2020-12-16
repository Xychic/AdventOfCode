import sys
import collections
import itertools
import numpy as np

inp = open(f"{sys.path[0]}/../input.txt").read()

rules, mine, other = [line.split("\n") for line in inp.split("\n\n")]
rules = [r.split(": ")[1] for r in rules]
rules = [r.split(" or ") for r in rules]
other = [o.split(",") for o in other[1:-1]]

for i, rule in enumerate(rules):
    newRule = []
    for j, subRule in enumerate(rule):
        a, b = subRule.split("-")
        newRule += [int(a), int(b)]
    rules[i] = newRule

invalid = []

for o in other:
    for val in o:
        if not any(((a <= int(val) <= b) or (c <= int(val) <= d)) for a,b,c,d in rules):
            invalid.append(int(val))
print(sum(invalid))