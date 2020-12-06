import sys
import collections
import itertools
import numpy as np
import string

questions = set(string.ascii_lowercase)
groups = []

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    if len(line):
        questions &= set(line)
    else:
        groups.append(questions)
        questions = set(string.ascii_lowercase)
groups.append(questions)

print(sum([len(g) for g in groups]))