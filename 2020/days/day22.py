import collections
import math
import os
import re
from functools import cache

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
""".strip()


def parse_deck(s):
    d = s.split("\n")
    return [int(c) for c in d[1:]][::-1]


def run_a(inp):
    a, b = inp.split("\n\n")
    a, b = parse_deck(a), parse_deck(b)

    while a and b:
        ac, bc = a.pop(), b.pop()
        if ac > bc:
            a = [bc, ac] + a
        else:
            b = [ac, bc] + b

    w = a or b

    score = sum(x * c for x, c in zip(w, range(1, len(w) + 1)))
    print(score)


memo = {}


def play(a, b):
    k0 = (tuple(a), tuple(b))
    if k0 in memo:
        return memo[k0]
    prev_rounds = set()
    while a and b:
        k = (tuple(a), tuple(b))
        # print(k)
        ac, bc = a.pop(), b.pop()
        if k in prev_rounds:
            a = [bc, ac] + a
            continue

        prev_rounds.add(k)

        if ac <= len(a) and bc <= len(b):
            a2, b2 = a[-ac:], b[-bc:]
            ra, rb = play(a2, b2)
            if ra:
                a = [bc, ac] + a
            else:
                b = [ac, bc] + b
        else:
            if ac > bc:
                a = [bc, ac] + a
            else:
                b = [ac, bc] + b

    memo[k0] = (a, b)
    return a, b


def run_b(inp):
    a, b = inp.split("\n\n")
    a, b = parse_deck(a), parse_deck(b)

    ra, rb = play(a, b)
    w = ra or rb
    score = sum((i + 1) * c for i, c in enumerate(w))
    print(score)


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
