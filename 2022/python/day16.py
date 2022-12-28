import os
import re
import time

from functools import cache
from typing import List, Tuple

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()

with open(f"input/day{DAY}-example.txt") as f:
    s1 = f.read().strip()


def tup(xs):
    return tuple(sorted(xs))


def parse(input):
    rates = dict()
    edges = dict()
    for line in input.split("\n"):
        src, *tgts = re.findall("[A-Z]{2}", line)
        edges[src] = tgts

        rate = int(re.search("\d+", line)[0])
        rates[src] = rate

    return edges, rates


def run(input):
    start = time.time()

    edges, rates = parse(input)

    working_valves = set(k for k, v in rates.items() if v > 0)

    @cache
    def rate(on):
        return sum(rates[k] for k in on)

    # off and on are sorted tuples representing sets
    @cache
    def search(on, curr: str, steps_left: int) -> Tuple[int, List[str]]:
        if steps_left == 0:
            return (0, [])

        rat = rate(on)

        if set(on) == working_valves:
            return (steps_left * rat, [])

        options = edges[curr]

        # if curr is a working valve and not currently on, then we need an
        # option to stay here and turn it on
        if curr in working_valves and curr not in on:
            options.append(curr)

        if not options:
            return (steps_left * rat, [])

        def run(n):
            new_on = tup(set([*on, curr])) if n == curr else on
            m, path = search(new_on, n, steps_left - 1)
            return (rat + m, [n] + path)

        return max(map(run, options), key=lambda p: p[0])

    mins = 30
    print(f"mins: {mins}")
    print(search((), "AA", mins))
    end = time.time()
    print(f"time: {round(end-start, 1)}s")


def run2(input):
    pass


print("=" * 40)
# run(s1)
run(real_input)  # took 7 minutes

# print()
# run2(s1)
# run2(real_input)
