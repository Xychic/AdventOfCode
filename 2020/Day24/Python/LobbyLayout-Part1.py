import sys
import collections
import itertools
import numpy as np

inp = """sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"""
inp = open(f"{sys.path[0]}/../input.txt").read()

def getPath(line):
    path = []
    dirs = ["se","sw","ne","nw","e","w"]
    while len(line):
        for d in dirs:
            if line.startswith(d):
                path.append(d)
                line = line[len(d):]
    return path

deltaDict = {
    "se" : (0, -1, 1),
    "sw" : (-1, 0, 1),
    "ne" : (1, 0, -1),
    "nw" : (0, 1, -1),
    "e"  : (1, -1, 0),
    "w"  : (-1, 1, 0)
}

tiles = collections.defaultdict(bool)

for line in inp.splitlines():
    x = y = z = 0
    for step in getPath(line):
        dx, dy, dz = deltaDict[step]
        x += dx
        y += dy
        z += dz
    tiles[(x,y,z)] = not tiles[(x,y,z)]

print(sum(tiles.values()))