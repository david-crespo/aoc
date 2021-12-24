import os

# import util as u
# from grid import Grid, pp, parse
# from parse import parse
# from collections import namedtuple, defaultdict, Counter
# from dataclasses import dataclass
# from functools import cache
# import math
# from typing import List, Tuple, Dict

s1 = """
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def parse_input(input):
    out = []
    for line in input.split("\n"):
        pass
    return out


def run(input):
    inp = parse_input(input)
    print(inp)


def run2(input):
    inp = parse_input(input)


print("=" * 40)
run(s1)
# run(real_input)

# print()
# run2(s1)
# run2(real_input)
