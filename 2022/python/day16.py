import os

# import util as u
# from grid import Grid, pp, parse
# from parse import parse
from collections import namedtuple, defaultdict, Counter

# from dataclasses import dataclass
from functools import cache

# import math
from typing import List, Tuple, Dict

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}-py.txt") as f:
    real_input = f.read().strip()

with open(f"input/day{DAY}-py-example.txt") as f:
    s1 = f.read().strip()


def parse_input(input):
    edges, rates = input.split("\n\n")
    edges = [e.split() for e in edges.split("\n")]
    edges = [(a, b, int(w)) for a, b, w in edges]

    edg = defaultdict(list)
    for a, b, w in edges:
        edg[a].append((b, w))

    rates = [r.split() for r in rates.split("\n")]
    rates = dict((n, int(w)) for n, w in rates)

    return edg, rates


def tup(xs):
    return tuple(sorted(xs))


def run(input):
    edges, rates = parse_input(input)

    # print(edges)
    # print()
    # print(rates)

    working_valves = tup(k for k, v in rates.items() if v > 0)

    @cache
    def rate(on):
        return sum(rates[k] for k in on)

    # off and on are sorted tuples representing sets
    @cache
    def search(off, on, curr: str, steps_left: int) -> Tuple[int, List[str]]:
        if steps_left == 0:
            return (0, [])

        rat = rate(on)

        if off == ():
            return (steps_left * rat, [])

        options = [(t, w) for t, w in edges[curr] if w <= steps_left]

        if not options:
            return (steps_left * rat, [])

        def run(n, w):
            m, path = search(
                # I think these are why my answer is too high. this makes it so
                # the valve goes into effect immediately. It needs to go into
                # effect one step after being turned on. this is pretty tough
                # with the abridged graph. turns out with all this memoization I
                # can probably just use the original and not even bother with
                # the A* BS
                tup(set(off) - set([curr])),
                tup(set([*on, curr])),
                n,
                steps_left - w,
            )
            return (rat * w + m, path + [n])

        return max((run(n, w) for n, w in options), key=lambda p: p[0])

    print(search(working_valves, (), "AA", 30))


def run2(input):
    inp = parse_input(input)


print("=" * 40)
run(s1)
# run(real_input)

# print()
# run2(s1)
# run2(real_input)
