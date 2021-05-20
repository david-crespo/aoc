import pytest
from aocd.models import Puzzle
from game import Game, parse_program

DAY_8_TEST_INPUT = """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""

DAY_8_TEST_PROGRAM = [
    ("nop", 0),
    ("acc", 1),
    ("jmp", 4),
    ("acc", 3),
    ("jmp", -3),
    ("acc", -99),
    ("acc", 1),
    ("jmp", -4),
    ("acc", 6),
]


def test_parse_program():
    assert parse_program(DAY_8_TEST_INPUT) == DAY_8_TEST_PROGRAM


def test_machine_day_8_sample():
    game = Game(DAY_8_TEST_PROGRAM)
    game.run()
    assert game.acc == 5
    assert game.halted == False


def test_machine_day_8_a():
    program = parse_program(Puzzle(year=2020, day=8).input_data)
    game = Game(program)
    game.run()
    assert game.acc == 2080
    assert game.halted == False


def test_machine_day_8_b():
    program = parse_program(Puzzle(year=2020, day=8).input_data)
    program[345] = ("nop", program[345][1])
    game = Game(program)
    game.run()
    assert game.acc == 2477
    assert game.halted == True
