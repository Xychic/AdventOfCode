import sys
import collections
import itertools
import numpy as np

inp = """Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"""

inp = open(f"{sys.path[0]}/../input.txt").read()

p1, p2 = inp.split("\n\n")
p1 = [int(a) for a in p1.splitlines()[1:]]
p2 = [int(a) for a in p2.splitlines()[1:]]

while p1 and p2:
    a = p1.pop(0)
    b = p2.pop(0)
    if a > b:
        p1 += [a, b]
    else:
        p2 += [b, a]

print(sum([(i+1) * v for i,v in enumerate((p1 + p2)[::-1])]))