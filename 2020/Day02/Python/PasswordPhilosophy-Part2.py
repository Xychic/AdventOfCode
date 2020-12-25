import sys
import collections
import itertools
import numpy as np
import re

total = 0

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    sLine = re.findall(r"[\w']+", line)
    a = int(sLine[0]) - 1
    b = int(sLine[1]) - 1
    c = sLine[2]
    d = sLine[3]
    
    if (d[a] == c) ^ (d[b] == c):
        total += 1

print(total)