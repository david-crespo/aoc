import os
import util as u

from collections import namedtuple

# from dataclasses import dataclass
# from functools import cache
# import math

sample_input = """
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
""".strip()

sample2 = """
11111
19991
19191
19991
11111
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def neighbors(pt, grid):
    [x, y] = pt
    if x > 0:
        yield (x - 1, y)
        if y > 0:
            yield (x - 1, y - 1)
        if y < grid.y_max:
            yield (x - 1, y + 1)
    if y > 0:
        yield (x, y - 1)
    if x < grid.x_max:
        yield (x + 1, y)
        if y > 0:
            yield (x + 1, y - 1)
        if y < grid.y_max:
            yield (x + 1, y + 1)
    if y < grid.y_max:
        yield (x, y + 1)


Grid = namedtuple("Grid", ["grid", "x_max", "y_max"])


def parse(input):
    lines = input.split("\n")
    grid = dict()
    for y, line in enumerate(lines):
        for x, d in enumerate(line):
            grid[(x, y)] = int(d)
    return Grid(grid, len(lines[0]) - 1, len(lines) - 1)


def pp(inp):
    for y in range(inp.y_max + 1):
        print("".join(str(inp.grid[(x, y)]) for x in range(inp.x_max + 1)))
    print()


def run(input):
    inp = parse(input)
    # pp(inp)
    flashes = 0
    for i in range(220):
        flashed = set()
        to_flash = set()
        for pt in inp.grid:
            inp.grid[pt] += 1
            if inp.grid[pt] > 9:
                to_flash.add(pt)
        while to_flash:
            for pt in to_flash:
                flashed.add(pt)
                for n in neighbors(pt, inp):
                    inp.grid[n] += 1
            to_flash = set()
            for pt, v in inp.grid.items():
                if v > 9 and pt not in flashed:
                    to_flash.add(pt)
        for pt in flashed:
            inp.grid[pt] = 0
        # pp(inp)
        flashes += len(flashed)

        # part 2
        if len(flashed) == len(inp.grid):
            print(f"all: {i + 1}")
            return
    # part 1
    # print(flashes)


def run2(input):
    inp = parse(input)


print("=" * 40)
run(sample_input)
# run(sample2)
run(real_input)

# print()
# run2(sample_input)
# run2(real_input)
