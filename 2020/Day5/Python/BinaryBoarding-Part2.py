import sys
import collections
import itertools
import numpy as np


def getSeatID(seat):
    seat = seat.replace("F","0").replace("B","1").replace("L", "0").replace("R","1")
    return int(seat, 2)

seats = []
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    seats.append(getSeatID(line))

allNum = [i for i in range(min(seats), max(seats))]

print([x for x in allNum if x not in seats][0])