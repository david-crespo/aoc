import os
import util as u

# from collections import namedtuple
# from dataclasses import dataclass
# from functools import cache
# import math

sample_input = """
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def parse(input):
    return input.split("\n")


OPEN = {"(": ")", "{": "}", "[": "]", "<": ">"}
CLOSE = {")": "(", "}": "{", "]": "[", ">": "<"}

SCORE = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137,
}


def run(input):
    inp = parse(input)
    score = 0
    for line in inp:
        stack = []
        for c in line:
            if c in OPEN:
                stack.append(c)
            elif stack[-1] == CLOSE[c]:
                stack.pop()
            else:
                # print("corrupted")
                score += SCORE[c]
                break

    print(score)


SCORE2 = {
    ")": 1,
    "]": 2,
    "}": 3,
    ">": 4,
}


def run2(input):
    inp = parse(input)
    scores = []
    for line in inp:
        stack = []
        bad = False
        for c in line:
            if c in OPEN:
                stack.append(c)
            elif stack[-1] == CLOSE[c]:
                stack.pop()
            else:
                bad = True
                break

        if not bad:
            score = 0
            for c in stack[::-1]:
                score *= 5
                score += SCORE2[OPEN[c]]
            scores.append(score)

    print(sorted(scores)[int(len(scores) / 2)])


print("=" * 40)
run(sample_input)
run(real_input)

print()
run2(sample_input)
run2(real_input)
