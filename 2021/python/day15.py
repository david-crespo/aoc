import os
from grid import Grid, neighbors4, pp, parse

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


def path_length(grid):
    G = nx.DiGraph()
    for pt in grid.pts:
        for n in neighbors4(pt, grid):
            G.add_edge(pt, n, weight=grid.pts[n])
    return nx.dijkstra_path_length(G, (0, 0), (grid.lx - 1, grid.ly - 1))


def run(input):
    grid = parse(input)
    print(path_length(grid))


def run2(input):
    inp = parse(input)

    grid = Grid(inp.pts.copy(), 5 * inp.lx, 5 * inp.ly)
    for i in range(5):
        for j in range(5):
            for x, y in inp.pts:
                new_pt = (x + inp.lx * i, y + inp.ly * j)
                v = inp.pts[(x, y)]
                inc = i + j
                while inc > 0:
                    v += 1
                    if v == 10:
                        v = 1
                    inc -= 1

                grid.pts[new_pt] = v
    print(path_length(grid))


print("=" * 40)
run(sample_input)
run(real_input)

print()
run2(sample_input)
run2(real_input)
