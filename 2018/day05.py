from aocd.models import Puzzle
import re
import string

TEST_INPUTS = [
    """
dabAcCaCBAcCcaDA
""",
]
TEST_INPUTS = [t.strip() for t in TEST_INPUTS]


alpha = string.ascii_lowercase
react_pairs = [x + y for x, y in zip(alpha, alpha.upper())]
react_pairs += [p[::-1] for p in react_pairs]
regex = re.compile("|".join(react_pairs))


def react(s):
    while regex.search(s):
        s = regex.sub("", s)
    return len(s)


def run_a(s):
    print("starting length:", len(s))
    ending_length = react(s)
    print(f"ending length: {ending_length}")


def run_b(s):
    print("starting length:", len(s))

    for letter in string.ascii_lowercase:
        new_s = re.sub(f"{letter}|{letter.upper()}", "", s)
        ending_length = react(new_s)
        print(letter, ending_length)


if __name__ == "__main__":
    input_data = Puzzle(year=2018, day=5).input_data
    run_a(TEST_INPUTS[0])
    print()
    run_a(input_data)

    print()
    run_b(TEST_INPUTS[0])
    print()
    run_b(input_data)
