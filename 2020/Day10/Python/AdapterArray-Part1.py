import sys
import collections
import itertools
import numpy as np

adapters = [0]
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    adapters.append(int(line))
adapters.sort()
adapters.append(max(adapters)+3)

diff = [(adapters[i+1] - adapters[i]) for i in range(len(adapters)-1)]
print(diff.count(1) * (diff.count(3)))