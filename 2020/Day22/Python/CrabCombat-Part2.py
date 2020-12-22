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

def recursiveGame(p1, p2):
    seen = collections.defaultdict(bool)
    while p1 and p2:
        game = (tuple(p1), tuple(p2))
        if seen[game]:
            return p1, True
        seen[game] = True

        a = p1.pop(0)
        b = p2.pop(0)

        pWin = a > b
        if len(p1) >= a and len(p2) >= b:
            _, pWin = recursiveGame([p1[i] for i in range(a)], [p2[i] for i in range(b)])
            
        if pWin:
            p1 += [a, b]
        else:
            p2  += [b ,a]
    
    return p1+p2, (p1 != [])

p1, p2 = inp.split("\n\n")
p1 = [int(a) for a in p1.splitlines()[1:]]
p2 = [int(a) for a in p2.splitlines()[1:]]

winner, _ = recursiveGame(p1, p2)
print(sum([(i+1) * v for i,v in enumerate(winner[::-1])]))