from aocd.models import Puzzle
import util as u
import collections
import math
import re
from os import system
import cProfile

SAMPLE = """
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
""".strip()


def can_sum(nums, target):
    return any(a != b and a + b == target for a in nums for b in nums)


def run_a(inp, pre_len):
    nums = list(map(int, inp.split("\n")))

    for i in range(pre_len, len(nums)):
        if not can_sum(nums[i - pre_len : i], nums[i]):
            print("invalid:", nums[i])


def run_b(inp, target):
    nums = list(map(int, inp.split("\n")))

    for l in range(2, 100):
        for i in range(len(nums) - l):
            chunk = nums[i : i + l]
            if sum(chunk) == target:
                print("answer:", min(chunk) + max(chunk))
                return
    print("failed")


if __name__ == "__main__":
    system("clear")
    inp = Puzzle(year=2020, day=9).input_data

    # print("=============== A ===============\n")
    # run_a(SAMPLE, 5)
    # print("\n" + "-" * 15 + "\n")
    # run_a(inp, 25)
    #
    # print("\n=============== B ===============\n")
    # run_b(SAMPLE, 127)
    # print("\n" + "-" * 15 + "\n")
    cProfile.run("run_b(inp, 15690279)")
