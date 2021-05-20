from aocd.models import Puzzle
import util as u
from collections import Counter

TEST_INPUTS = [
    """
abc

a
b
c

ab
ac

a
a
a
a

b
""",
]
TEST_INPUTS = [t.strip() for t in TEST_INPUTS]


def run_a(inp):
    groups = [Counter(s.replace("\n", "")) for s in inp.split("\n\n")]
    print(sum(len(g.keys()) for g in groups))


def run_b(inp):
    groups = [
        (Counter(s.replace("\n", "")), len(s.split("\n"))) for s in inp.split("\n\n")
    ]

    print(sum(u.count(lambda v: v == n, c.values()) for c, n in groups))


if __name__ == "__main__":
    inp = Puzzle(year=2020, day=6).input_data
    u.run_for_inputs(TEST_INPUTS + [inp], run_a, "A")
    u.run_for_inputs(TEST_INPUTS + [inp], run_b, "B")
