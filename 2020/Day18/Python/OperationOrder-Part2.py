import sys
import collections
import itertools
import numpy as np

def evaluate(data):
    while "(" in data:
        start = data.index("(")
        opened = 1
        for i, c in enumerate(data[start+1:]):
            if c == "(":
                opened += 1
            elif c == ")":
                opened -=1
            if opened == 0:
                end = start+1+i
                val = evaluate(data[start+1:end])
                data = data[:start] + [val] + data[end+1:]
                break
    while "+" in data:
        pos = data.index("+")
        a = data[pos-1]
        c = data[pos+1]
        data = data[:pos-1] + [int(a) + int(c)] + data[pos+2:]
    
    while len(data) > 1:
        a, b, c = data[:3]
        val = int(a) * int(c)
        data = [val] + data[3:]
    return data[0]

total = 0
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    total += evaluate(list(line.replace(" ","")))
print(total)