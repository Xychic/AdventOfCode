import sys
import collections
import itertools
import numpy as np

cardKey, doorKey = [int(x) for x in open(f"{sys.path[0]}/../input.txt").read().splitlines()]
# cardKey, doorKey = 5764801, 17807724

doorLoop = 0
doorVal = 1
while doorVal != doorKey:
    doorVal *= 7
    doorVal %= 20201227
    doorLoop += 1

cardLoop = 0
cardVal = 1
while cardVal != cardKey:
    cardVal *= 7
    cardVal %= 20201227
    cardLoop += 1

print(cardLoop, doorLoop)
print(pow(doorKey, cardLoop, 20201227))