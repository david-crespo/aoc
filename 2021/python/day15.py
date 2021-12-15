import os

# import util as u
# from parse import parse
from collections import namedtuple, defaultdict, Counter
import networkx as nx

sample_input = """
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()

Grid = namedtuple("Grid", ["grid", "x_max", "y_max"])


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


def parse_input(input):
    grid = dict()
    lines = input.split("\n")
    for y, line in enumerate(lines):
        for x, v in enumerate(line):
            grid[(x, y)] = int(v)

    return Grid(grid, len(lines[0]) - 1, len(lines) - 1)


def path_length(grid):
    G = nx.DiGraph()
    for pt in grid.grid:
        for n in neighbors(pt, grid):
            G.add_edge(pt, n, weight=grid.grid[n])
    return nx.dijkstra_path_length(G, (0, 0), (grid.x_max, grid.y_max))


def run(input):
    grid = parse_input(input)
    print(path_length(grid))


def pp(inp):
    for y in range(inp.y_max + 1):
        print("".join(str(inp.grid[(x, y)]) for x in range(inp.x_max + 1)))
    print()


def run2(input):
    inp = parse_input(input)

    grid = Grid(inp.grid.copy(), 5 * (inp.x_max + 1) - 1, 5 * (inp.y_max + 1) - 1)
    for i in range(5):
        for j in range(5):
            for x, y in inp.grid:
                new_pt = (x + (inp.x_max + 1) * i, y + (inp.y_max + 1) * j)
                v = inp.grid[(x, y)]
                inc = i + j
                while inc > 0:
                    v += 1
                    if v == 10:
                        v = 1
                    inc -= 1

                grid.grid[new_pt] = v
    print(path_length(grid))


print("=" * 40)
run(sample_input)
run(real_input)

print()
run2(sample_input)
run2(real_input)
