import sys
import collections
import itertools
import numpy as np
import math

time, buses = open(f"{sys.path[0]}/../input.txt").read().splitlines()

def modularInverse(a, b):
    return pow(a, b-2, b)

timeTable = []
M = 1
for delay, bus in enumerate(buses.split(",")):
    if bus != 'x':
        bus = int(bus)
        timeTable.append((bus, delay))
        M *= bus

print(M)

# https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html
SUM = 0
for bus, delay in timeTable:
    ai = bus - (delay % bus)
    bi = M // bus
    biInv = modularInverse(bi, bus)
    print(biInv, bi, bus)
    SUM += ai * bi * biInv

print(SUM % M)