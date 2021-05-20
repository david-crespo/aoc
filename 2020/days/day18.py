import collections
import math
import os
import re
from functools import cache
from operator import add, mul

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

ops = {"*": mul, "+": add}


def ev_ltr(exps):
    if isinstance(exps, int):
        return exps
    if len(exps) == 1:
        return ev_ltr(exps[0])

    a, op, b, *rest = exps
    return ev_ltr([op(ev_ltr(a), ev_ltr(b)), *rest])


def ev_add_first(exps):
    if isinstance(exps, int):
        return exps
    if len(exps) == 1:
        return ev_add_first(exps[0])

    op_idx = exps.index(add) if add in exps else exps.index(mul)
    head, a, op, b, rest = (
        exps[: op_idx - 1],
        exps[op_idx - 1],
        exps[op_idx],
        exps[op_idx + 1],
        exps[op_idx + 2 :],
    )
    # print(head, a, op, b, rest)
    return ev_add_first([*head, op(ev_add_first([a]), ev_add_first([b])), *rest])


def get_exp_op_rest(s):
    if s.startswith("("):
        open_parens = 1
        i = 1
        while open_parens > 0:
            if s[i] == "(":
                open_parens += 1
            elif s[i] == ")":
                open_parens -= 1
            i += 1
        exp_end = i
    elif " " in s:
        exp_end = s.index(" ")
    else:
        exp_end = len(s)

    exp, op, rest = s[:exp_end], None, None
    if exp_end < len(s):
        op, rest = s[exp_end + 1], s[exp_end + 3 :]
    return exp, op, rest


def parse(inp):
    # print("inp:", inp)
    if inp in ops:
        return ops[inp]
    if re.match("^\d+$", inp):
        return int(inp)

    first_exp, _, _ = get_exp_op_rest(inp)
    if first_exp == inp:
        inp = inp[1:-1]

    rest = inp
    exps = []

    while rest:
        a, op, rest = get_exp_op_rest(rest)
        exps.append(a)
        if op:
            exps.append(op)

    # print(exps)
    return list(map(parse, exps))


def run_a(inp):
    inps = inp.split("\n")
    total = 0
    for i in inp.split("\n"):
        # print(i)
        exps = parse(i)
        total += ev_ltr(exps)
    print(total)


def run_b(inp):
    inps = inp.split("\n")
    total = 0
    for i in inp.split("\n"):
        # print(i)
        exps = parse(i)
        # print(exps)
        total += ev_add_first(exps)
    print(total)

    pass


if __name__ == "__main__":
    os.system("clear")
    inp0 = Puzzle(year=2020, day=DAY).input_data

    print("=============== A ===============\n")
    run_a("2 * 3 + (4 * 5)")
    print("\n" + "-" * 15 + "\n")
    run_a("5 + (8 * 3 + 9 + 3 * 4 * 3)")
    print("\n" + "-" * 15 + "\n")
    run_a("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
    print("\n" + "-" * 15 + "\n")
    run_a("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    print("\n" + "-" * 15 + "\n")
    run_a(inp0)

    print("\n=============== B ===============\n")
    run_b("2 * 3 + (4 * 5)")
    print("\n" + "-" * 15 + "\n")
    run_b("5 + (8 * 3 + 9 + 3 * 4 * 3)")
    print("\n" + "-" * 15 + "\n")
    run_b("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
    print("\n" + "-" * 15 + "\n")
    run_b("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    print("\n" + "-" * 15 + "\n")
    run_b(inp0)
