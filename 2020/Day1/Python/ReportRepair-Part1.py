import sys
import collections
import itertools

values = []

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    values.append(int(line))

x = itertools.combinations(values, 2)
for a, b  in x:
    if a + b == 2020:
        print(a * b)
        break