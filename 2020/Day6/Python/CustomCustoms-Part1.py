import sys
import collections
import itertools
import numpy as np

questions = set()
groups = []

for line in open(f"{sys.path[0]}/../input.txt").read().splitlines():
    if len(line):
        questions.update(set(line))
    else:
        groups.append(questions)
        questions = set()
groups.append(questions)

print(sum([len(g) for g in groups]))