import sys
import collections
import itertools
import numpy as np

values = []

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    values.append(int(line))

x = itertools.combinations(values, 3)
for c in x:
    if sum(list(c)) == 2020:
        print(np.prod(c))
        break