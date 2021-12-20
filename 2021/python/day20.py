import os
from grid import parse
from functools import cache

sample_input = """
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def parse_input(input):
    alg, rest = input.split("\n\n")
    return [[int(c == "#") for c in alg], rest]


def pp(grid):
    min_x = min(x for x, y in grid.keys())
    min_y = min(y for x, y in grid.keys())
    max_x = max(x for x, y in grid.keys())
    max_y = max(y for x, y in grid.keys())
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            print("#" if grid[(x, y)] else ".", end="")
        print()
    print()


@cache
def nbs(pt):
    [x, y] = pt
    return [
        (x - 1, y - 1),
        (x, y - 1),
        (x + 1, y - 1),
        (x - 1, y),
        (x, y),
        (x + 1, y),
        (x - 1, y + 1),
        (x, y + 1),
        (x + 1, y + 1),
    ]


@cache
def to_int(bits):
    return int("".join(str(b) for b in bits), 2)


def enhance(grid, alg, void=0):
    min_x = min(x for x, y in grid.keys())
    min_y = min(y for x, y in grid.keys())
    max_x = max(x for x, y in grid.keys())
    max_y = max(y for x, y in grid.keys())
    new_grid = dict()
    for x in range(min_x - 10, max_x + 10):
        for y in range(min_y - 10, max_y + 10):
            idx = to_int(tuple(grid.get(pt, void) for pt in nbs((x, y))))
            new_grid[(x, y)] = alg[idx]
    return new_grid


def run(input):
    alg, grid = parse_input(input)
    grid = parse(grid.replace("#", "1").replace(".", "0")).pts
    for i in range(2):
        # print(i)
        grid = enhance(grid, alg, void=alg[0] if i % 2 == 1 else 0)
    print(len([v for v in grid.values() if v]))


def run2(input):
    alg, grid = parse_input(input)
    grid = parse(grid.replace("#", "1").replace(".", "0")).pts
    for i in range(50):
        print(i)
        grid = enhance(grid, alg, void=alg[0] if i % 2 == 1 else 0)
    print("result:", len([v for v in grid.values() if v]))


print("=" * 40)
run(sample_input)

# 5479 is not correct
run(real_input)

print()
run2(sample_input)
run2(real_input)
