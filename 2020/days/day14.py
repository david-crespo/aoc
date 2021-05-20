import collections
import math
import os
import re

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
""".strip()

SAMPLE_2 = """
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
""".strip()


def run_a(inp):
    mask = None
    data = {}
    for line in inp.split("\n"):
        if line.startswith("mask"):
            mask = line[7:]
            continue
        i, n = re.match("mem\[(\d+)\] = (.+)", line).groups()
        i, n = int(i), int(n)

        value = bin(n)[2:]
        value = (36 - len(value)) * "0" + value
        masked = "".join(m if m != "X" else v for v, m in zip(value, mask))
        data[i] = int(masked, 2)
    print(sum(data.values()))


def mask_address_bit(a, m):
    if m == "0":
        return a
    if m == "1":
        return "1"
    if m == "X":
        return "X"


def replace_at(s, i, v):
    return s[:i] + v + s[i + 1 :]


def unmask(s):
    if "X" not in s:
        return [s]
    i = s.index("X")
    return unmask(replace_at(s, i, "0")) + unmask(replace_at(s, i, "1"))


def run_b(inp):
    mask = None
    data = {}
    for line in inp.split("\n"):
        if line.startswith("mask"):
            mask = line[7:]
            continue
        i, n = re.match("mem\[(\d+)\] = (.+)", line).groups()
        i, n = int(i), int(n)

        address = bin(i)[2:]
        address = (36 - len(address)) * "0" + address
        masked_address = "".join(mask_address_bit(a, m) for a, m in zip(address, mask))
        addresses = unmask(masked_address)
        for a in addresses:
            data[a] = n
    print(sum(data.values()))


if __name__ == "__main__":
    os.system("clear")
    inp = Puzzle(year=2020, day=DAY).input_data

    print("=============== A ===============\n")
    run_a(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run_a(inp)

    print("\n=============== B ===============\n")
    run_b(SAMPLE_2)
    print("\n" + "-" * 15 + "\n")
    run_b(inp)
