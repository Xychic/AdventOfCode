import sys
import collections
import itertools
import numpy as np

inp = "389125467"
inp = open(f"{sys.path[0]}/../input.txt").read().strip()
data = [int(a) for a in inp]
current = data[0]
for move in range(100):
    # print(f"\nmove: {move+1}")
    # print(f"current: {current}")
    # print(f"cups: {data}")
    destination = current -1
    pick = [data[(data.index(current) + i) % len(data)] for i in range(1, 4)]
    for p in pick:
        data.remove(p)
    # print(f"pickup: {pick}")

    while destination not in data:
        if destination == 0:
            destination = max(data)
        else:
            destination -= 1
    # print(f"desination: {destination}")
    insert = data.index(destination)+1
    for v in pick[::-1]:
        data.insert(insert, v)
    current = data[(data.index(current)+1) % len(data)]

start = data.index(1)
print("".join(str(d) for d in data[start+1:] + data[:start]))
