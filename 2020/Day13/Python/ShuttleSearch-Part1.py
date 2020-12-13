import sys
import collections
import itertools
import numpy as np

time, buses = open(f"{sys.path[0]}/../input.txt").read().splitlines()

time = int(time)
bestTime = float("inf")
bstBus = 0
for bus in filter(lambda x: x != "x", buses.split(",")):
    bus = int(bus)
    delay = bus - (time % bus)
    if delay < bestTime:
        bestTime = delay
        bestBus = bus

print(bestBus * bestTime)