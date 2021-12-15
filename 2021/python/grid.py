from collections import namedtuple

Grid = namedtuple("Grid", ["pts", "lx", "ly"])


def parse(input):
    pts = dict()
    lines = input.split("\n")
    for y, line in enumerate(lines):
        for x, v in enumerate(line):
            pts[(x, y)] = int(v)
    return Grid(pts, len(lines[0]), len(lines))


def pp(grid):
    for y in range(grid.ly):
        for x in range(grid.lx):
            print(grid.pts[(x, y)], end="")
        print()
    print()


def neighbors4(pt, grid):
    [x, y] = pt
    if x > 0:
        yield (x - 1, y)
    if y > 0:
        yield (x, y - 1)
    if x < grid.lx - 1:
        yield (x + 1, y)
    if y < grid.ly - 1:
        yield (x, y + 1)


def neighbors8(pt, grid):
    [x, y] = pt
    if x > 0:
        yield (x - 1, y)
        if y > 0:
            yield (x - 1, y - 1)
        if y < grid.ly - 1:
            yield (x - 1, y + 1)
    if y > 0:
        yield (x, y - 1)
    if x < grid.lx - 1:
        yield (x + 1, y)
        if y > 0:
            yield (x + 1, y - 1)
        if y < grid.ly - 1:
            yield (x + 1, y + 1)
    if y < grid.ly - 1:
        yield (x, y + 1)
