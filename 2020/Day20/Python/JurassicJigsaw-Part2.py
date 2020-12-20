import sys
import collections
import itertools
import numpy as np

inp = """Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."""

inp = open(f"{sys.path[0]}/../input.txt").read()

class tile:
    def __init__(self, name, tile):
        self.tile = tile
        self.name = name
        self.sides = self.getSides()
        self.allSides = self.sides.copy()
        for s in self.sides:
            self.allSides.append(s[::-1])
        self.matchedTile = []
        self.matchedSides = []
        self.pos = ()
    def getSide(self, d):
        # d = [(0,1), (1,0), (0,-1), (-1,0)].index(dir)
        if d == 3: 
            return list(self.tile[0])
        elif d == 2:
            return list(self.tile[:, 0])[::-1]
        elif d == 1:
            return list(self.tile[-1])[::-1]
        elif d == 0:
            return list(self.tile[:, -1])
    
    def getSides(self):
        return [self.getSide(i) for i in range(4)]

    def rotate(self):
        self.tile = np.rot90(self.tile)

    def flip(self):
        self.tile = np.array([s[::-1] for s in self.tile])
        self.sides = self.getSides()

    
tiles = []

for line in inp.strip().split("\n\n"):
    data = line.split("\n")
    tileName = int(data[0].split()[1][:-1])
    tiles.append(
        tile(
            tileName, 
            np.array(
                [list(row) for row in data[1:]]
            )
        )
    )

for i, tileA in enumerate(tiles):
    for j, tileB in enumerate(tiles):
        if i == j:
            continue
        matches = [a for a in tileA.sides if a in tileB.allSides]
        if len(matches) > 0 :
            tileA.matchedTile.append(tileB)
        tileA.matchedSides.extend(matches)

toPlace = tiles.copy()
placed = []
for t in tiles:
    if len(t.matchedSides) == 2:
        current = t
        break

blank = np.array([["?" for _ in range(9)] for _ in range(10)])
world = [[blank for _ in range(12)] for _ in range(12)]

def showWorld():
    for y in world:
        for row, _ in enumerate(y[0]):
            for x in y:
                print("".join(x[row]),end=" ")
            print()
        print()
# 0 = N
# 1 = W
# 2 = S
# 3 = E
s = [current.allSides.index(side) % 4 for side in current.matchedSides]

y = 11 if 0 in s else 0
x = 11 if 1 in s else 0
world[y][x] = current.tile
current.pos = (y, x)
# for t in current.matchedTile:
#     print(t.matchedSides)

minX = minY = 0
maxX = maxY = 11
pathLen = 11
dirs = [(0,1), (1,0), (0,-1), (-1,0)]
dirIndex = 1

def getNextPiece(side, d):
    for pos in tiles:
        for _ in range(4):
            if side == pos.getSides()[(d+2) % 4]:
                return pos

            pos.flip()
            if side == pos.getSides()[(d+2) % 4]:
                return pos
            pos.flip()

            pos.rotate()

t = np.array([[x + y for x in "abcd"] for y in "1234"])


for _ in range(pathLen):
    tiles.remove(current)
    dy, dx = dirs[dirIndex]
    y, x = y+dy, x+dx
    # showWorld()
    nextPiece = getNextPiece(current.getSide(dirIndex)[::-1], dirIndex)
    world[y][x] = nextPiece.tile
    current = nextPiece
dirIndex = (dirIndex + 1) % 4
    
while pathLen > 0:
    for _ in range(2):
        for _ in range(pathLen):
            tiles.remove(current)
            dy, dx = dirs[dirIndex]
            y, x = y+dy, x+dx
            # showWorld()
            nextPiece = getNextPiece(current.getSide(dirIndex)[::-1], dirIndex)
            world[y][x] = nextPiece.tile
            current = nextPiece

        dirIndex = (dirIndex + 1) % len(dirs)
    pathLen -= 1
    
for i, y in enumerate(world):
    for j, x in enumerate(y):
        world[i][j] = x[1:-1,1:-1]

showWorld()
flattenedWorld = []
for y in world:
    for row, _ in enumerate(y[0]):
        line = ""
        for x in y:
            line += "".join(x[row])
        flattenedWorld.append(list(line))

def markMonsters(flattenedWorld):
    x, y = 0, 0
    monsters = 0
    shape = [(0,18), (1,0), (1, 5), (1,6), (1,11), (1,12), (1,17), (1,18), (1,19), (2,1), (2,4), (2,7), (2,10), (2,13), (2,16)]

    rows = len(flattenedWorld)
    cols = len(flattenedWorld[0])

    for y in range(rows-3):
        while x < cols - 20:

            if all([flattenedWorld[y+dy][x+dx] == "#" for dy,dx in shape]):
                monsters += 1
                for dy, dx in shape:
                    flattenedWorld[y+dy][x+dx] = "O "
                x += 20
            else:
                x += 1

        x = 0
    return monsters     

for _ in range(2):
    for _ in range(4):
        if markMonsters(flattenedWorld):
            break
        flattenedWorld = np.rot90(flattenedWorld)
    else:
        flattenedWorld = [row[::-1] for row in flattenedWorld]

for row in flattenedWorld:
    print("".join(row))

print(sum("".join(row).count('#') for row in flattenedWorld))