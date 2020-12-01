import sys
import collections
import itertools

values = []

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    values.append(int(line))

combi = itertools.combinations(values, 3)
for a, b, c in combi:
    if a + b + c == 2020:
        print(a * b * c)
        break