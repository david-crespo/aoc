import collections
import math
import os
import re
from functools import cache

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
""".strip()


def parse_line(s):
    ing, allerg = s[:-1].split(" (contains ")
    return set(ing.split(" ")), set(allerg.split(", "))


def run_a(inp):
    data = list(map(parse_line, inp.split("\n")))
    poss = collections.defaultdict(set)
    for ing, allerg in data:
        for a in allerg:
            if a in poss:
                poss[a] &= ing
            else:
                poss[a].update(ing)

    poss_allergens = set().union(*poss.values())
    all_ing = set().union(*(ing for ing, _ in data))
    not_poss_allergens = all_ing - poss_allergens
    count = 0
    for ing, _ in data:
        for i in ing:
            if i in not_poss_allergens:
                count += 1
    print(count)


def run_b(inp):
    data = list(map(parse_line, inp.split("\n")))
    poss = collections.defaultdict(set)
    for ing, allerg in data:
        for a in allerg:
            if a in poss:
                poss[a] &= ing
            else:
                poss[a].update(ing)

    poss_allergens = set().union(*poss.values())
    all_ing = set().union(*(ing for ing, _ in data))
    not_poss_allergens = all_ing - poss_allergens
    allergens = all_ing - not_poss_allergens

    mapping = {}
    while len(mapping) < len(allergens):
        k, i = next((k, idxs.pop()) for k, idxs in poss.items() if len(idxs) == 1)
        mapping[k] = i
        for k, idxs in poss.items():
            poss[k].discard(i)
    print(mapping)

    x = ",".join(y[1] for y in sorted(mapping.items(), key=lambda y: y[0]))
    print(x)


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
