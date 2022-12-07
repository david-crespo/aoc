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
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def tuple_starts_with(t, prefix):
    if len(t) < len(prefix):
        return False

    for x, y in zip(t, prefix):
        if x != y:
            return False

    return True


def parse_input(input):
    out = []
    curr_path = []
    tree = dict()

    def move_up():
        path = tuple(curr_path)
        childrenSum = sum(
            tree[k]
            for k in tree.keys()
            if len(k) == len(path) + 1 and tuple_starts_with(k, path)
        )
        tree[path] += childrenSum
        curr_path.pop()

    for line in input.split("\n"):
        # print(curr_path)
        if line.startswith("$ cd"):
            d = line[5:]
            if d == "..":
                move_up()
            else:
                curr_path.append(d)
                path = tuple(curr_path)
                tree[path] = 0
        elif line.startswith("$ ls"):
            pass
        elif line.startswith("dir "):
            pass
        else:  # file
            size, name = line.split(" ")
            path = tuple(curr_path)
            tree[path] += int(size)

    while curr_path:
        move_up()

    return tree


def run(input):
    tree = parse_input(input)
    print(sum(v for v in tree.values() if v <= 100000))


def run2(input):
    tree = parse_input(input)
    free_space = 70000000 - tree[("/",)]
    min_delete = 30000000 - free_space
    print(sorted(v for v in tree.values() if v >= min_delete)[0])


print("=" * 40)
run(s1)
run(real_input)

print()
run2(s1)
run2(real_input)
