from aocd.models import Puzzle
import util as u
import collections
import math
import re
import os
from itertools import groupby

DAY = int(os.path.basename(__file__)[3:5])

SAMPLE = """
16
10
15
5
1
11
7
19
6
12
4
""".strip()

SAMPLE_2 = """
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
""".strip()


# initially I did this a naive way where I looped through and picked the
# min of the available next ones, etc. etc. I didn't realize that I had
# to use all the adapters, which just meant sorting them and counting the
# gaps. that's why I got my worst ranking yet for part 1 and caught up a lot
# in part 2. luckily this was a clue for how to do part 2 — the relevant thing
# is the sequences of gaps
def run_a(inp):
    nums = list(sorted(map(int, inp.split("\n"))))
    nums = [0, *nums, max(nums) + 3]

    c = collections.Counter(b - a for a, b in u.consec_pairs(nums))
    print(c)
    print("1s x 3s:", c[1] * c[3])


# number of perms for n consecutive 1-gaps
# per explanation below, number of excluded cases is number of ways of
# arranging gaps of bigger than 3, which is 1 + 2 + ... + n-3.
# for n = 4 it's 1, so 8 - 1 = 7
# for n < 4 the number excluded is 0 so it's just 2 ** (n-1)
#
# realized later that this doesn't work for n >= 6 or maybe 7 because
# there's room for two illegal gaps, so there's an additional term needed
# or maybe a whole series
def perms(n):
    return 2 ** (n - 1) - sum(range(1, (n - 3) + 1))


# solution relies on the fact that there are only gaps of size 1 and 3
#
# if the number of consecutive 1-gaps is n = 1, 2, or 3, the possibilities are simple:
# the last one has to be there because it's 3 below the next one. and the one before
# the first 1-gap is only 3 below the last one, i.e., the one after the last 1-gap.
# so the perms are simply 2^(n - 1) because any of them could be there or not
#
# with a sequence of 4 1-gaps (5 consecutive nums) the situation is slightly different
# because there's one case that's not allowed: all 4 being gone. so you subtract 1
# from 2^(n-1) = 2^(4-1) = 2^3 = 8
#
# if there was a sequence of 5 1-gaps, I guess you would have to rule out 3, so it
# would be 2^4 - 3 = 13 you rule out 1-2-3-4-5, 1-2-3-4, and 2-3-4-5 all being left out
def run_b(inp):
    nums = [0] + list(sorted(map(int, inp.split("\n"))))
    gaps = [b - a for a, b in zip(nums, nums[1:])]
    one_gap_group_lens = [len(list(g)) for k, g in groupby(gaps) if k == 1]
    print(math.prod(perms(l) for l in one_gap_group_lens))


# this is me trying to write ray's beautiful solution from memory
def run_ray(inp):
    nums = list(sorted(map(int, inp.split("\n"))))
    end = max(nums) + 3
    nums.append(end)
    paths_to = {0: 1}
    for n in nums:
        paths_to[n] = sum(paths_to.get(n - i, 0) for i in [1, 2, 3])
    print(paths_to[end])


if __name__ == "__main__":
    os.system("clear")
    inp = Puzzle(year=2020, day=DAY).input_data

    print("=============== A ===============\n")
    run_a(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run_a(SAMPLE_2)
    print("\n" + "-" * 15 + "\n")
    run_a(inp)

    print("\n=============== B ===============\n")
    run_b(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run_b(SAMPLE_2)
    print("\n" + "-" * 15 + "\n")
    run_b(inp)

    print("\n=============== Ray ===============\n")
    run_ray(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run_ray(SAMPLE_2)
    print("\n" + "-" * 15 + "\n")
    run_ray(inp)

    print("\n=============== DP ===============\n")
    run_dp(SAMPLE)
    print("\n" + "-" * 15 + "\n")
    run_dp(SAMPLE_2)
    # print("\n" + "-" * 15 + "\n")
    # run_dp(inp)
