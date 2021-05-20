from aocd.models import Puzzle
import util as u

TEST_INPUTS = [
    """
FBFBBFFRLR
""",
]
TEST_INPUTS = [t.strip() for t in TEST_INPUTS]


def parse(s):
    row = int(s[:7].replace("F", "0").replace("B", "1"), 2)
    col = int(s[7:].replace("L", "0").replace("R", "1"), 2)
    return (row, col, row * 8 + col)


def run_a(input_data):
    lines = input_data.split("\n")
    passes = map(parse, lines)
    print(max(p[2] for p in passes))


def run_b(input_data):
    lines = input_data.split("\n")
    passes = sorted(map(parse, lines), key=lambda p: p[2])
    pairs = zip(passes, passes[1:])
    print([p for p in pairs if p[1][2] - p[0][2] == 2])


if __name__ == "__main__":
    input_data = Puzzle(year=2020, day=5).input_data

    # u.run_for_inputs(TEST_INPUTS, run_a, "A")
    u.run_for_inputs(TEST_INPUTS + [input_data], run_a, "A")
    # u.run_for_inputs(TEST_INPUTS, run_b, "B")
    u.run_for_inputs(TEST_INPUTS + [input_data], run_b, "B")
