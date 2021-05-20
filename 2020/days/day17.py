import collections
import math
import os
import re
from functools import cache

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
.#.
..#
###
""".strip()

ds = [-1, 0, 1]


def active_neighbors(pt, board):
    x, y, z = pt
    active = 0
    for dx in ds:
        x1 = x + dx
        for dy in ds:
            y1 = y + dy
            for dz in ds:
                z1 = z + dz
                if not (dx == dy == dz == 0):
                    npt = (x1, y1, z1)
                    if board.get(npt):
                        active += 1
    return active


def run_a(inp):
    n = 9
    board = {}
    for z in range(-n, n + 1):
        for y in range(-n, n + 1):
            for x in range(-n, n + 1):
                board[(x, y, z)] = 0

    for y, row in enumerate(inp.split("\n")):
        offset = len(row) // 2
        for x, c in enumerate(row):
            board[(x - offset, y - offset, 0)] = int(c == "#")

    for i in range(6):
        new_board = board.copy()
        for pt, v in board.items():
            ns = active_neighbors(pt, board)
            if v and not (ns == 2 or ns == 3):
                new_board[pt] = 0
            elif not v and ns == 3:
                new_board[pt] = 1
        board = new_board

    print(sum(new_board.values()))


@cache
def neighbors4(pt):
    x, y, z, w = pt
    result = []
    for dx in ds:
        for dy in ds:
            for dz in ds:
                for dw in ds:
                    npt = (x + dx, y + dy, z + dz, w + dw)
                    if npt != pt:
                        result.append(npt)
    return result


def run_b(inp):
    n = 9
    board = {}
    for w in range(-n, n + 1):
        for z in range(-n, n + 1):
            for y in range(-n, n + 1):
                for x in range(-n, n + 1):
                    board[(x, y, z, w)] = 0

    for y, row in enumerate(inp.split("\n")):
        offset = len(row) // 2  # basically the initial occupied radius
        for x, c in enumerate(row):
            board[(x - offset, y - offset, 0, 0)] = int(c == "#")

    for i in range(6):
        print(i)
        radius = offset + i + 1
        new_board = board.copy()
        for pt, v in board.items():
            # optimization: avoid looking at pts far from center
            if any(abs(d) > radius for d in pt):
                continue
            ns = sum(1 for npt in neighbors4(pt) if board.get(npt))
            if v and not (ns == 2 or ns == 3):
                new_board[pt] = 0
            elif not v and ns == 3:
                new_board[pt] = 1
        board = new_board

    print("answer:", sum(new_board.values()))


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
