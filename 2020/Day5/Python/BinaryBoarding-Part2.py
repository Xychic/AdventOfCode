import sys
import collections
import itertools
import numpy as np


def getSeatID(seat):
    seat = seat.replace("F","0").replace("B","1").replace("L", "0").replace("R","1")
    return int(seat, 2)

allNum = [i for i in range(2**10)]

high = 0
low = float("inf")
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    sID = getSeatID(line)
    high = max(sID, high)
    low = min(sID, low)
    allNum.remove(sID)

ans = [x for x in allNum if (low < x < high)][0]

print(ans)