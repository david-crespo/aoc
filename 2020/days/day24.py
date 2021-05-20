import collections
import math
import os
import re
from functools import cache

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
sesenwnenenewseeswwswswwnenewsewsw
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
wseweeenwnesenwwwswnew
""".strip()


def to_pt(s):
    moves = re.findall("(e|se|sw|w|nw|ne)", s)
    x, y = (0, 0)
    for m in moves:
        if m == "e":
            x += 1
        elif m == "se":
            y -= 1
            x += 1
        elif m == "sw":
            y -= 1
        elif m == "w":
            x -= 1
        elif m == "nw":
            y += 1
            x -= 1
        elif m == "ne":
            y += 1
    return x, y


def run_a(inp):
    tiles = list(map(to_pt, inp.split("\n")))
    c = collections.Counter(tiles)
    print(sum(1 for k, v in c.items() if v % 2 == 1))


@cache
def neighbors(pt):
    x, y = pt
    return [
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1),
        (x - 1, y + 1),
        (x + 1, y - 1),
    ]


def step(d):
    new_d = dict()
    for x in range(-80, 80):
        for y in range(-80, 80):
            pt = (x, y)
            curr = d.get(pt, 0)
            ct = sum(1 for n in neighbors(pt) if d.get(n, 0) == 1)
            if curr and (ct == 0 or ct > 2):  # black
                new_d[pt] = 0
            elif not curr and ct == 2:
                new_d[pt] = 1
            else:
                new_d[pt] = curr
    return new_d


def run_b(inp):
    tiles = list(map(to_pt, inp.split("\n")))
    c = collections.Counter(tiles)
    d = dict((k, 1) for k, v in c.items() if v % 2 == 1)

    for i in range(100):
        d = step(d)
        print(f"{i + 1}\t{len([k for k, v in d.items() if v == 1])}")


if __name__ == "__main__":
    os.system("clear")
    inp = Puzzle(year=2020, day=DAY).input_data

    print("=============== A ===============\n")
    run_a("esew")
    run_a("nwwswee")
    run_a(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run_a(inp)

    print("\n=============== B ===============\n")
    run_b(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run_b(inp)
