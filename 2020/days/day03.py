from aocd.models import Puzzle
import util as u
import Pt
import math

TEST_INPUTS = [
    """
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
""",
]
TEST_INPUTS = [t.strip() for t in TEST_INPUTS]


def tree_at(grid, pt):
    x, y = pt
    width = len(grid[0])
    adj_x = x % width
    return grid[y][adj_x] == "#"


def count_trees(grid, slope):
    pt = (0, 0)
    path_length = len(grid) // slope[1]
    path = Pt.path((0, 0), slope, path_length)
    return u.count(lambda pt: tree_at(grid, pt), path)


def run_a(input_data):
    lines = input_data.split("\n")
    print(len(lines), "lines")

    tree_count = count_trees(lines, (3, 1))
    print("tree count:", tree_count)


def run_b(input_data):
    lines = input_data.split("\n")
    print(len(lines), "lines")

    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

    counts = [count_trees(lines, slope) for slope in slopes]
    print(counts)
    print("product:", math.prod(counts))


if __name__ == "__main__":
    input_data = Puzzle(year=2020, day=3).input_data
    # u.run_for_inputs(TEST_INPUTS, run_a, "A")

    u.run_for_inputs(TEST_INPUTS + [input_data], run_a, "A")
    # u.run_for_inputs(TEST_INPUTS, run_b, "B")
    u.run_for_inputs(TEST_INPUTS + [input_data], run_b, "B")
