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
# inp = open(f"{sys.path[0]}/../input.txt").read()

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

# print((minX, maxX), (minY, maxY), (minZ, maxZ))
for day in range(100):
    nextTiles = tiles.copy()
    for x in range(minX-1, maxX+2):
        for y in range(minY-1, maxY+2):
            for z in range(minZ-1, maxZ+1):
                nextTo = sum([tiles[(x+dx, y+dy, z+dz)] for dx, dy, dz in deltaDict.values()])
                if tiles[(x,y,z)]:
                    if nextTo == 0:
                        nextTiles[(x,y,z)] = False
                    elif nextTo > 2:
                        nextTiles[(x,y,z)] = False
                elif not tiles[(x,y,z)] and (nextTo == 2):
                    nextTiles[(x,y,z)] = True
    tiles = nextTiles.copy()
    for x,y,z in tiles.keys():
        minX = min(minX, x-1)
        maxX = max(maxX, x+1)
        minY = min(minY, y-1)
        maxY = max(maxY, y+1)
        minZ = min(minZ, z-1)
        maxZ = max(maxZ, z+1)
    print(f"Day {day+1}: {sum(tiles.values())}")