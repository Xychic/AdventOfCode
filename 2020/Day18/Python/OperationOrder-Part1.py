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
    
    while len(data) > 1:
        a, b, c = data[:3]
        if b == "+":
            val = int(a) + int(c)
        elif b == "*":
            val = int(a) * int(c)
        data = [val] + data[3:]
    return data[0]

total = 0
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    total += evaluate(list(line.replace(" ","")))
print(total)