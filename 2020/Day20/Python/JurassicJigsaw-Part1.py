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
        self.sides = []
        self.allSides = []
        self.name = name
        self.sides.append(list(tile[0]))
        self.sides.append(list(tile[:, 0]))
        self.sides.append(list(tile[-1]))
        self.sides.append(list(tile[:, -1]))
        for s in self.sides:
            self.allSides.append(s[::-1])
        self.allSides.extend(self.sides)
        self.matches = 0
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
    for tileB in tiles[i+1:]:
        matches = len([a for a in tileA.sides if a in tileB.allSides])
        tileA.matches += matches
        tileB.matches += matches

print(np.prod([t.name for t in tiles if t.matches == 2]))