import collections
import math
import os
import re
from functools import cache

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
Tile 2311:
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
..#.###...
""".strip()

Edges = collections.namedtuple("Edges", ["top", "right", "bottom", "left"])


def parse(inp):
    tiles = dict()
    for part in inp.split("\n\n"):
        lines = part.split("\n")
        num = int(lines[0][5:9])
        tile = lines[1:]
        edges = Edges(
            tile[0],
            "".join(r[-1] for r in tile),
            tile[-1][::-1],
            "".join(r[0] for r in reversed(tile)),
        )
        tiles[num] = (tile, edges)
    return tiles


def get_matching_edges(tiles):
    matching_edges = collections.defaultdict(list)
    for t, (_, edges) in tiles.items():
        for d, e in edges._asdict().items():
            k = (t, d)
            for t0, (_, edges0) in tiles.items():
                for d0, e0 in edges0._asdict().items():
                    k0 = (t0, d0)
                    if t != t0 and (e == e0 or e == e0[::-1]):
                        matching_edges[k].append(k0)
    return matching_edges


def run_a(inp):
    tiles = parse(inp)
    matching_edges = get_matching_edges(tiles)

    nums = [k[0] for k in matching_edges]
    c = collections.Counter(nums)

    print(math.prod(k for k, v in c.items() if v == 2))


def ppd(d):
    for k, v in d.items():
        print(k, v)


def ppg(g):
    print()
    for row in g:
        print(" ".join(map(str, row)))
    print()


def ppt(t):
    for r in t:
        print(r)
    print()


def remove_tile(t, d):
    for k in d:
        d[k].discard(t)


def corners(me):
    return dict((k, v) for k, v in me.items() if len(v) == 2)


# 1951    2311    3079
# 2729    1427    2473
# 2971    1489    1171

monster = ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]
monster_pts=[]
for y, row in enumerate(monster):
    for x, c in enumerate(row):
        if c == "#":
            monster_pts.append((x,y))


def count_monsters(grid):
    count = 0
    monster_w, monster_h = len(monster[0]), len(monster)
    for y in range(len(grid) - (monster_h - 1)):
        for x in range(len(grid) - (monster_w - 1)):
            if all(grid[y + my][x + mx] == "#" for (mx, my) in monster_pts):
                count += 1
    return count


def flip(tile):
    return [r[::-1] for r in tile]


def chop(tile):
    return [r[1:-1] for r in tile[1:-1]]


def rotate(tile):
    new_tile = []
    side_len = len(tile)
    for x in range(side_len):
        new_tile.append("".join(tile[y][x] for y in range(side_len)))
    return flip(new_tile)


def run_b(inp):
    tiles = parse(inp)
    side_len = int(math.sqrt(len(tiles)))
    matching_edges = get_matching_edges(tiles)

    me = collections.defaultdict(set)
    for (num, side), v in matching_edges.items():
        me[num].add(v[0][0])

    grid = [[None for i in range(side_len)] for j in range(side_len)]

    first_corner = next(k for k in corners(me))
    grid[0][0] = first_corner
    remove_tile(first_corner, me)

    outer = set(k for k, v in me.items() if len(v) == 2 or len(v) == 3)

    # fill in top edge
    for x in range(1, side_len):
        prev = grid[0][x - 1]
        nxt = list(me[prev] & outer)[0]
        remove_tile(nxt, me)
        grid[0][x] = nxt

    # filling in the rest is easy because the cell above the cursor will
    # only ever have one neighbor left
    for y in range(1, side_len):
        for x in range(0, side_len):
            above = grid[y - 1][x]
            nxt = me[above].pop()
            remove_tile(nxt, me)
            grid[y][x] = nxt

    # ppg(grid)

    me = collections.defaultdict(dict)
    dirs = collections.defaultdict(dict)
    for (num, side), v in matching_edges.items():
        me[num][side] = v[0]
        dirs[num][v[0][0]] = side

    # ppd(me)
    # ppd(dirs)

    ang = dict(top=0, right=1, bottom=2, left=3)

    # figure out rotation and flipping by comparing actual direction
    # of some other tile with the direction of the corresponding matching
    # edge. figure out whether we need to flip by doing this for two edges
    # (can't be opposite edges) and seeing if their implied rotation is
    # different (flip) or the same (no flip)
    final_grid = []
    for y in range(side_len):
        row = []
        for x in range(side_len):
            curr = grid[y][x]
            if x < side_len - 1:
                right = grid[y][x + 1]
                right_dir = dirs[curr][right]
                xdiff = (ang["right"] - ang[right_dir]) % 4
                print(x, y, curr, f"{right=}", right_dir, xdiff)
            else:
                left = grid[y][x - 1]
                left_dir = dirs[curr][left]
                xdiff = (ang["left"] - ang[left_dir]) % 4
                print(x, y, curr, f"{left=}", left_dir, xdiff)

            if y < side_len - 1:
                bottom = grid[y + 1][x]
                bottom_dir = dirs[curr][bottom]
                ydiff = (ang["bottom"] - ang[bottom_dir]) % 4
                print(x, y, curr, f"{bottom=}", bottom_dir, ydiff)
            else:
                top = grid[y - 1][x]
                top_dir = dirs[curr][top]
                ydiff = (ang["top"] - ang[top_dir]) % 4
                print(x, y, curr, f"{top=}", top_dir, ydiff)
            print()

            tile = tiles[curr][0]
            for i in range(ydiff):
                tile = rotate(tile)
            if xdiff != ydiff:
                tile = flip(tile)
            # ppt(tile)
            row.append(chop(tile))
        final_grid.append(row)

    ppg(grid)

    final = ["".join(t) for row in final_grid for t in zip(*row)]
    # final = flip(final)
    # for i in range(3):
    #     final = rotate(final)

    pound_ct = collections.Counter(''.join(final))['#']
    # print(pound_ct)

    # loop through orientations, count monsters, stop when you find any
    for i in range(2):
        final = flip(final)
        for i in range(4):
            final = rotate(final)
            ct = count_monsters(final)
            if ct > 0:
                print("answer:", pound_ct - ct *len(monster_pts))
                return


if __name__ == "__main__":
    os.system("clear")
    inp = Puzzle(year=2020, day=DAY).input_data

    print("=============== A ===============\n")
    run_a(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run_a(inp)

    print("\n=============== B ===============\n")
    run_b(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run_b(inp)
