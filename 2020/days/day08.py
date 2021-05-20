from aocd.models import Puzzle
import util as u
import collections
import math
import re
from os import system

from game import Game, parse_program


TEST_INPUTS = [
    """
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
""",
]
TEST_INPUTS = [t.strip() for t in TEST_INPUTS]


def run_a(inp):
    program = parse_program(inp)
    m = Game(program)
    m.run()
    print(m.acc)


def run_b(inp):
    program = parse_program(inp)
    cmd_map = dict(jmp="nop", nop="jmp", acc="acc")
    for i, (cmd, arg) in enumerate(program):
        new_program = program[:]
        new_program[i] = (cmd_map[cmd], arg)

        m = Game(new_program)
        m.run()
        if m.halted:
            print(m.acc)


if __name__ == "__main__":
    system("clear")
    inp = Puzzle(year=2020, day=8).input_data
    u.run_for_inputs(TEST_INPUTS + [inp], run_a, "A")
    u.run_for_inputs(TEST_INPUTS + [inp], run_b, "B")
