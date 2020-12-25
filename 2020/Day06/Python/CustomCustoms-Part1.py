import sys
import collections
import itertools
import numpy as np

groups = []

for line in open(f"{sys.path[0]}/../input.txt").read().split("\n\n"):
    groups.append(set(line.replace("\n","")))

print(sum([len(g) for g in groups]))