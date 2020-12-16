import sys
import collections
import itertools
import numpy as np

toRead = []
target = 2020
lastSeen = [(0,0)] * target
lastSaid = 0

for line in open(f"{sys.path[0]}/../input.txt").read().split(","):
    toRead.append(int(line))

for i in range(1, target+1):
    a, b = lastSeen[lastSaid]
    if toRead:
        lastSaid = toRead.pop(0)
    elif lastSeen[lastSaid][0] == 0:
        lastSaid = 0
    else:
        lastSaid = b - a
    lastSeen[lastSaid] = (b, i)
print(i, lastSaid)