import sys
import collections
import itertools
import numpy as np

adapters = [0]
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    adapters.append(int(line))
adapters.sort()
adapters.append(max(adapters)+3)

seen = {}
def countAdapters(ad):
    if ad == adapters[-1]:
        return 1
    elif ad in seen:
        return seen[ad]
    total = 0
    for j in range(adapters.index(ad)+1, len(adapters)):
        if (adapters[j] - ad) <= 3:
            total += countAdapters(adapters[j])
    seen[ad] = total
    return total
    
print(countAdapters(0))