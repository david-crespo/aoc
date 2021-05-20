from aocd.models import Puzzle
import util as u
import re
from collections import defaultdict
from os import system

TEST_INPUTS = [
    """
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
""",
    """
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
""",
]
TEST_INPUTS = [t.strip() for t in TEST_INPUTS]


def parse_child(s):
    n, color = re.sub(" bags?", "", s).split(" ", maxsplit=1)
    return (int(n), color)


def parse_line(s):
    parent, rest = s.rstrip(".").split(" bags contain ", maxsplit=1)
    if rest == "no other bags":
        return (parent, [])

    return (parent, list(map(parse_child, rest.split(", "))))


def get_parents_for_color(rules, child):
    return set(p for p, cs in rules if any(c[1] == child for c in cs if c))


def run_a(inp):
    lines = inp.split("\n")
    rules = list(map(parse_line, lines))

    to_check = get_parents_for_color(rules, "shiny gold")
    possible_parents = set()

    while to_check:
        possible_parents.update(to_check)
        checking = to_check.pop()
        ps = get_parents_for_color(rules, checking)
        to_check.update(ps)

    print(len(possible_parents))


def run_b(inp):
    lines = inp.split("\n")
    rules = dict(map(parse_line, lines))

    to_add = rules.get("shiny gold")
    contains = defaultdict(int)
    while to_add:
        count, color = to_add.pop()
        contains[color] += count

        children = rules.get(color)
        if children:
            to_add += [(n * count, c_color) for n, c_color in children]

    print(sum(contains.values()))


if __name__ == "__main__":
    system("clear")
    inp = Puzzle(year=2020, day=7).input_data
    # u.run_for_inputs(TEST_INPUTS, run_a, "A")
    u.run_for_inputs(TEST_INPUTS + [inp], run_a, "A")
    # u.run_for_inputs(TEST_INPUTS, run_b, "B")
    u.run_for_inputs(TEST_INPUTS + [inp], run_b, "B")


# IT'S NOT 27
# IT'S NOT 176
