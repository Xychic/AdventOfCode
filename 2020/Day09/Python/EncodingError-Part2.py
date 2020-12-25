import sys
import collections
import itertools
import numpy as np

numbers = []
for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    numbers.append(int(line))

target = 0

for i in range(25, len(numbers)):
    num = numbers[i]
    for a, b in itertools.combinations(numbers[i-25:i], 2):
        if a + b == num:
            break
    else:
        target = num
        break

low = high = 0

while True:
    run = numbers[low:high]
    if sum(run) < target:
        high += 1
    elif target < sum(run):
        low += 1
    else:
        break

print(min(run) + max(run))