import collections
import math
import os
import re
import cProfile
from functools import cache

from aocd.models import Puzzle
import util as u
import Grid

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
""".strip()

DIRS = [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)]


def adj_occupied(board, x, y, max_x, max_y):
    ns = 0
    if x > 0 and board[y][x - 1] == "#":
        ns += 1
    if y > 0 and board[y - 1][x] == "#":
        ns += 1
    if x < max_x and board[y][x + 1] == "#":
        ns += 1
    if y < max_y and board[y + 1][x] == "#":
        ns += 1
    if x > 0 and y > 0 and board[y - 1][x - 1] == "#":
        ns += 1
    if y > 0 and x < max_x and board[y - 1][x + 1] == "#":
        ns += 1
    if x < max_x and y < max_y and board[y + 1][x + 1] == "#":
        ns += 1
    if y < max_y and x > 0 and board[y + 1][x - 1] == "#":
        ns += 1
    return ns


def step_a(board):
    new_board = [list(r) for r in board]
    w, h = len(board[0]), len(board)
    for x in range(w):
        for y in range(h):
            c = board[y][x]
            if c == ".":
                continue

            n = adj_occupied(board, x, y, w - 1, h - 1)
            if c == "L" and n == 0:
                new_board[y][x] = "#"
            elif c == "#" and n >= 4:
                new_board[y][x] = "L"
    return ["".join(r) for r in new_board]


@cache
def path(x, y, d, max_x, max_y):
    dx, dy = d
    x, y = x + dx, y + dy

    p = []
    while x <= max_x and x >= 0 and y <= max_y and y >= 0:
        p.append((x, y))
        x, y = x + dx, y + dy
    return p


# los = line of sight
def los_occupied(board, x, y, max_x, max_y):
    ns = 0
    for d in DIRS:
        p = path(x, y, d, max_x, max_y)
        for a, b in p:
            seat = board[b][a]
            if seat == "#":
                ns += 1
                break
            elif seat == "L":
                break
    return ns


def step_b(board):
    new_board = []
    max_x, max_y = len(board[0]) - 1, len(board) - 1
    for y, r in enumerate(board):
        new_board.append(list(r))
        for x, c in enumerate(r):
            if c == ".":
                continue

            n = los_occupied(board, x, y, max_x, max_y)
            if c == "L" and n == 0:
                new_board[y][x] = "#"
            if c == "#" and n >= 5:
                new_board[y][x] = "L"
    return ["".join(r) for r in new_board]


def run_until_stable(board, step):
    b1 = None
    b2 = board
    i = 1
    while not b1 == b2:
        if i % 10 == 0:
            print(i, "steps")
        b1 = b2
        b2 = step(b2)
        i += 1

    print(i - 1, "steps")
    print("answer:", sum(c == "#" for r in b2 for c in r))


def run_a(inp):
    board = inp.split("\n")
    run_until_stable(board, step_a)


def run_b(inp):
    board = inp.split("\n")
    run_until_stable(board, step_b)


if __name__ == "__main__":
    os.system("clear")
    inp = Puzzle(year=2020, day=DAY).input_data

    print("=============== A ===============\n")
    run_a(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    # cProfile.run("run_a(inp)")
    run_a(inp)

    print("\n=============== B ===============\n")
    run_b(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    # # cProfile.run("run_b(inp)")
    run_b(inp)
