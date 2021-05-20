import collections
import math
import os
import re

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
F10
N3
F7
R90
F11
""".strip()


def parse_line(s):
    return (s[0], int(s[1:]))


def parse(inp):
    lines = inp.split("\n")
    return list(map(parse_line, lines))


dsr = dict(E="S", S="W", W="N", N="E")
dsl = dict(E="N", S="E", W="S", N="W")


def run_a(inp):
    parsed = parse(inp)

    facing = "E"
    x = 0
    y = 0
    for d, l in parsed:
        if d == "E":
            x += l
        elif d == "N":
            y += l
        elif d == "W":
            x -= l
        elif d == "S":
            y -= l
        elif d == "R":
            for i in range(l // 90):
                facing = dsr[facing]
        elif d == "L":
            for i in range(l // 90):
                facing = dsl[facing]
        elif d == "F":
            if facing == "E":
                x += l
            elif facing == "N":
                y += l
            elif facing == "W":
                x -= l
            elif facing == "S":
                y -= l
        # print((d, l), (x, y))
    print(x, y, abs(x) + abs(y))


def run_b(inp):
    parsed = parse(inp)

    x = 0
    y = 0
    xw = 10
    yw = 1
    for d, l in parsed:
        if d == "E":
            xw += l
        elif d == "N":
            yw += l
        elif d == "W":
            xw -= l
        elif d == "S":
            yw -= l
        elif d == "R":
            for i in range(l // 90):
                xw, yw = yw, -xw
        elif d == "L":
            for i in range(l // 90):
                xw, yw = -yw, xw
        elif d == "F":
            for i in range(l):
                x += xw
                y += yw
        # print((d, l), (x, y), (xw, yw))
    print(x, y, abs(x) + abs(y))


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
