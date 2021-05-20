import collections
import math
import os
import re

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
""".strip()


SAMPLE_2 = """
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
""".strip()


def parse_rule(s):
    k, rs = s.split(": ")
    rs = rs.split(" or ")
    rs = [tuple(map(int, r.split("-"))) for r in rs]
    return k, rs


def test_rule(v, r):
    k, rs = r
    return any(v >= low and v <= high for low, high in rs)


def is_valid(t, rules):
    for n in t:
        valid = any(test_rule(n, r) for r in rules)
        if not valid:
            return False
    return True


def parse_ticket(t):
    return tuple(map(int, t.split(",")))


def parse(inp):
    rules, my_ticket, nearby = inp.split("\n\n")
    my_ticket = parse_ticket(my_ticket.split("\n")[1])
    rules = list(map(parse_rule, rules.split("\n")))
    nearby = list(map(parse_ticket, nearby.split("\n")[1:]))
    return rules, my_ticket, nearby


def run_a(inp):
    rules, my_ticket, nearby = parse(inp)

    error = 0
    for t in nearby:
        for n in t:
            valid = any(test_rule(n, r) for r in rules)
            if not valid:
                error += n

    print(error)


def run_b(inp):
    rules, my_ticket, nearby = parse(inp)

    valid = [t for t in nearby if is_valid(t, rules)]

    poss = collections.defaultdict(set)
    for r in rules:
        k, rs = r
        for i in range(len(nearby[0])):
            if all(test_rule(t[i], r) for t in valid):
                poss[k].add(i)

    mapping = {}
    while len(mapping) < len(my_ticket):
        k, i = next((k, idxs.pop()) for k, idxs in poss.items() if len(idxs) == 1)
        mapping[k] = i
        for k, idxs in poss.items():
            poss[k].discard(i)

    answer = 1
    for k, i in mapping.items():
        if k.startswith("departure"):
            answer *= my_ticket[i]
    print(answer)


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
