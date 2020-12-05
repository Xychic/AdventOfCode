import sys
import collections
import itertools
import numpy as np


def getSeatID(seat):
    seat = seat.replace("F","0").replace("B","1").replace("L", "0").replace("R","1")
    return int(seat, 2)
    
best = 0
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    best = max(getSeatID(line), best)

print(best)
