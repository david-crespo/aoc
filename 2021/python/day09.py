from collections import namedtuple
import math

sample_input = """
2199943210
3987894921
9856789892
8767896789
9899965678
""".strip()

Grid = namedtuple("Grid", ["grid", "x_max", "y_max"])

with open("input/day09.txt") as f:
    real_input = f.read().strip()


def neighbors(pt, grid):
    [x, y] = pt
    if x > 0:
        yield (x - 1, y)
    if y > 0:
        yield (x, y - 1)
    if x < grid.x_max:
        yield (x + 1, y)
    if y < grid.y_max:
        yield (x, y + 1)


def parse(input):
    out = {}
    lines = input.split("\n")
    for y, line in enumerate(lines):
        for x, d in enumerate(line):
            out[(x, y)] = int(d)
    return Grid(out, len(lines[0]) - 1, len(lines) - 1)


def low_points(inp):
    for pt in inp.grid:
        value = inp.grid[pt]
        ns = [inp.grid[n] for n in neighbors(pt, inp)]
        if all(value < n for n in ns):
            yield pt


def run(input):
    inp = parse(input)
    print(sum(inp.grid[pt] + 1 for pt in low_points(inp)))


def get_basin(pt, inp):
    larger = [
        n for n in neighbors(pt, inp) if inp.grid[n] > inp.grid[pt] and inp.grid[n] != 9
    ]
    return set([pt] + [p for n in larger for p in get_basin(n, inp)])


def run2(input):
    inp = parse(input)
    low = low_points(inp)
    basins = [get_basin(pt, inp) for pt in low_points(inp)]
    basin_sizes = [len(b) for b in sorted(basins, key=len)]
    print(math.prod(basin_sizes[-3:]))


print("=" * 40)
run(sample_input)
run(real_input)

print()
run2(sample_input)
run2(real_input)
