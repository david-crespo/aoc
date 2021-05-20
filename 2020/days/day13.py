import collections
import math
import os
import re

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
939
7,13,x,x,59,x,31,19
""".strip()

SAMPLE_2 = """
939
17,x,13,19
""".strip()
SAMPLE_3 = """
939
1789,37,47,1889
""".strip()


def run_a(inp):
    t, bs = inp.split("\n")
    t = int(t)
    bs = [int(b) for b in bs.split(",") if b != "x"]

    answer = None
    for b in bs:
        x = b
        while x < t:
            x += b

        if not answer or x < answer:
            answer = x
            print(b, answer, (answer - t) * b)


def run_for(bs, n=1, base=None, interval=None):
    first = bs[0]
    bs = sorted(bs, key=lambda b: -b[1])

    biggest = bs[0][1]
    bi = bs[0][0]

    solutions = []
    t = -bi if not base else base
    if not interval:
        interval = biggest * first if bi == first else biggest
    while True:
        t += interval
        bad = False
        for i, b in bs:
            if not (t + i) % b == 0:
                bad = True
                break
        if not bad:
            solutions.append(t)
            if len(solutions) >= n:
                return solutions


# the trick, which I only figured out after 90 minutes, is the relationship
# between sub-lists and the full list. Consecutive solutions to the same list
# are separated by a constant interval. to find that interval just get two solutions
# instead of one and subtract.
#
# if list A is the first n items of list B
# call sol1A and sol2A the first two solutions for list A
# call intA = sol2A - sol1A
# all solutions for A take the form sol1A + m * intA for m = 1..infinity
#
# because list B contains list A, all solutions for B will also have to be solutions
# for list A, which means they must look like sol1A + m * intA also!
# this means you can use sol1A as a base and intA as interval for searching
# for solutions for list b. you have to choose a large enough sublist that intA is
# really big so you cover ground fast. half was not big enough: interval was ~10^8
# and we know from the problem that the solution is > 10^15. taking 2/3 of the list
# worked, interval was ~10^10. second pass is near instant
def run_b(inp):
    _, bs = inp.split("\n")
    bs = [(i, int(b)) for i, b in enumerate(bs.split(",")) if b != "x"]

    sublist = bs[: len(bs) * 2 // 3]

    sol1, sol2 = run_for(sublist, 2)
    interval = sol2 - sol1

    print("sublist sol:", sol1)
    print("sublist interval:", interval)

    # I found out later that this whole problem is the chinese remainder theorem,
    # one of the consequences of which is that the interval I empirically calculated
    # by finding two solutions and subtracting can be gotten more easily by
    # multiplying all the numbers in the sublist
    # https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html
    print("product of nums in sublist:", math.prod(b for i, b in sublist))

    print("answer:", run_for(bs, 1, sol1, interval)[0])


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
    run_b(SAMPLE_2)
    print("\n" + "-" * 15 + "\n")
    run_b(SAMPLE_3)
    print("\n" + "-" * 15 + "\n")
    run_b(inp)
