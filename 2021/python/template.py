import os
import util as u

# from collections import namedtuple
# from dataclasses import dataclass
# from functools import cache
# import math

sample_input = """
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def parse(input):
    out = []
    for line in input.split("\n"):
        pass
    return out


def run(input):
    inp = parse(input)
    print(inp)


def run2(input):
    inp = parse(input)


print("=" * 40)
run(sample_input)
# run(real_input)

# print()
# run2(sample_input)
# run2(real_input)
