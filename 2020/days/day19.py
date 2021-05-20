import collections
import math
import os
import re
from functools import cache

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb
""".strip()

sample2 = """
42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
""".strip()


# "abc" -> [("a", "bc"), ("ab", "c")]
@cache
def splits(s):
    return [(s[:l], s[l:]) for l in range(1, len(s))]


@cache
def matches(s, rule):
    if rule == '"a"':
        return s == "a"

    if rule == '"b"':
        return s == "b"

    if re.match("^\d+$", rule):
        return matches(s, rules[rule])

    if "|" in rule:
        a, b = rule.split(" | ")
        return matches(s, a) or matches(s, b)

    head, rest = rule.split(" ", maxsplit=1)
    for left, right in splits(s):
        if matches(left, head) and matches(right, rest):
            return True

    return False


def parse(inp):
    rules, msgs = inp.split("\n\n")
    rules = dict(r.split(": ") for r in rules.split("\n"))
    msgs = msgs.split("\n")
    return rules, msgs


def run(inp, part2mod=False):
    # had to make rules global so matches would have a nice cacheable interface
    global rules
    rules, msgs = parse(inp)

    if part2mod:
        rules["8"] = "42 | 42 8"
        rules["11"] = "42 31 | 42 11 31"

    matching = 0
    for m in msgs:
        # print(m)
        if matches(m, rules["0"]):
            matching += 1
    print(matching)
    matches.cache_clear()


if __name__ == "__main__":
    os.system("clear")
    inp = Puzzle(year=2020, day=DAY).input_data

    print("=============== A ===============\n")
    run(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run(inp)

    print("\n=============== B ===============\n")
    run(sample2, part2mod=True)
    print("\n" + "-" * 15 + "\n")
    run(inp, part2mod=True)
