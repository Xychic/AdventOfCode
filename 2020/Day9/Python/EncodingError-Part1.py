import sys
import collections
import itertools
import numpy as np

numbers = []
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    numbers.append(int(line))

for i in range(25, len(numbers)):
    num = numbers[i]
    for a, b in itertools.combinations(numbers[i-25:i], 2):
        if a + b == num:
            break
    else:
        print(num)
        break
