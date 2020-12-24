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
minX = minY = minZ = 0
maxX = maxY = maxZ = 0
for line in inp.splitlines():
    x = y = z = 0
    for step in getPath(line):
        dx, dy, dz = deltaDict[step]
        x += dx
        y += dy
        z += dz
    minX = min(minX, x)
    maxX = max(maxX, x)
    minY = min(minY, y)
    maxY = max(maxY, y)
    minZ = min(minZ, z)
    maxZ = max(maxZ, z)
    tiles[(x,y,z)] = not tiles[(x,y,z)]

for day in range(100):
    newTiles = collections.defaultdict(bool)
    toCheck = []
    for (x, y, z) in tiles:
        if tiles[(x,y,z)]:
            toCheck.append((x,y,z))
            for dx, dy, dz in deltaDict.values():
                toCheck.append((x+dx, y+dy, z+dz))
    for (x, y, z) in toCheck:
        nextTo = sum([tiles[(x+dx, y+dy, z+dz)] for dx, dy, dz in deltaDict.values()])
        if tiles[(x,y,z)] and not (nextTo == 0 or nextTo > 2):
            newTiles[(x,y,z)] = True
        if not tiles[(x,y,z)] and nextTo == 2:
            newTiles[(x,y,z)] = True
    tiles = newTiles
    print(f"Day {day+1}: {sum(tiles.values())}")