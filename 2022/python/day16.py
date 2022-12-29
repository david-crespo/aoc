import os
import re
import time
import cProfile

from functools import cache
from typing import List, Tuple, Dict, Set

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()

with open(f"input/day{DAY}-example.txt") as f:
    s1 = f.read().strip()


def tup(xs):
    return tuple(sorted(xs))


def parse(input: str) -> Tuple[Dict[str, Set[str]], Dict[str, int]]:
    rates = dict()
    edges = dict()
    for line in input.split("\n"):
        src, *tgts = re.findall("[A-Z]{2}", line)
        edges[src] = set(tgts)

        rate = int(re.search("\d+", line)[0])
        rates[src] = rate

    return edges, rates


def run(input):
    start = time.time()

    edges, rates = parse(input)

    working_valves = set(k for k, v in rates.items() if v > 0)
    working_valves_tup = tup(working_valves)

    @cache
    def rate(on):
        return sum(rates[k] for k in on)

    # off and on are sorted tuples representing sets
    @cache
    def search(on, curr: str, steps_left: int) -> Tuple[int, List[str]]:
        if steps_left == 0:
            return (0, [])

        rat = rate(on)

        if on == working_valves_tup:
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


@cache
def stup(a, b):
    """Sorted tuple"""
    return (a, b) if a < b else (b, a)


@cache
def get_options(options1, options2):
    return set(stup(o1, o2) for o1 in options1 for o2 in options2)


@cache
def add_on(on, to_add):
    return tup(set([*on, to_add]))


def run2(input):
    start = time.time()

    edges, rates = parse(input)

    working_valves = set(k for k, v in rates.items() if v > 0)
    working_valves_tup = tup(working_valves)

    working_valves_adj = working_valves | set(
        k for k, v in edges.items() for n in v if n in working_valves
    )
    working_valves_non_adj = set(edges.keys()) - working_valves_adj

    @cache
    def rate(on):
        return sum(rates[k] for k in on)

    # off and on are sorted tuples representing sets
    @cache
    def search(
        on: Tuple, curr: Tuple[str, str], steps_left: int
    ) -> Tuple[int, List[Tuple[str, str]]]:
        if steps_left == 0:
            return (0, [])

        rat = rate(on)

        # no point moving or turning on with one step left
        if steps_left == 1 or on == working_valves_tup:
            return (steps_left * rat, [])

        curr1, curr2 = curr

        turn_on_1 = curr1 in working_valves and curr1 not in on
        turn_on_2 = curr2 in working_valves and curr2 not in on

        def run(o: Tuple[str, str]):
            o1, o2 = o
            new_on = on
            if (o1 == curr1 and turn_on_1) or (o1 == curr2 and turn_on_2):
                new_on = add_on(new_on, o1)
            if (o2 == curr1 and turn_on_1) or (o2 == curr2 and turn_on_2):
                new_on = add_on(new_on, o2)
            m, path = search(new_on, o, steps_left - 1)
            return (m, [o] + path)

        # if there are two steps left, the only thing you can do
        # is stay on a valve that's not on yet to open it
        if steps_left == 2:
            extra = 0
            if turn_on_1:
                extra += rates[curr1]
            if turn_on_2:
                extra += rates[curr2]

            return (2 * rat + extra, [curr])

        options1 = edges[curr1].copy()
        options2 = edges[curr2].copy()

        # if curr is a working valve and not currently on, then we need an
        # option to stay here and turn it on
        if turn_on_1:
            options1.add(curr1)

        if turn_on_2:
            options2.add(curr2)

        # tried better optimizations for 3 steps left, but the following helps a
        # little and is simple. you can only go to a working valve or one away
        # from a working valve
        if steps_left == 3:
            options1 -= working_valves_non_adj
            options2 -= working_valves_non_adj

        options = get_options(tup(options1), tup(options2))

        # exclude the case where both are staying put on the same spot, only one
        # needs to do that
        if curr1 == curr2:
            options.discard((curr1, curr1))

        if not options:
            return (steps_left * rat, [])

        # doing the (rat+) here instead of run saved time
        m, path = max(map(run, options), key=lambda p: p[0])
        return (rat + m, path)

    mins = 16
    print(f"mins: {mins}")
    print(search((), ("AA", "AA"), mins))
    end = time.time()
    print(f"time: {round(end-start, 1)}s")


print("=" * 40)
# run(s1)
# run(real_input)  # took 7 minutes

# print()
# run2(s1)
run2(real_input)  # took 1039.3s
