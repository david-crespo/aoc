from aocd.models import Puzzle
import util as u

TEST_INPUTS = [
    """
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
""",
]
TEST_INPUTS = [t.strip() for t in TEST_INPUTS]


def valid_password_a(line):
    rng, letter, pw = line.split(" ")
    letter = letter[:1]
    minc, maxc = map(int, rng.split("-"))
    letter_count = u.count(letter, pw)
    # print(f"{rng=} {letter=} {pw=} {minc=} {maxc=} {letter_count=}")
    return letter_count >= minc and letter_count <= maxc


def valid_password_b(line):
    pos, letter, pw = line.split(" ")
    letter = letter[:1]
    pos1, pos2 = [int(m) - 1 for m in pos.split("-")]

    pos1has = pw[pos1] == letter
    pos2has = pw[pos2] == letter
    answer = pos1has != pos2has  # xor
    # print()
    # print(line)
    # print(f"{pos1=}\t{pw[pos1]=}\t{pos2=}\t{pw[pos2]=}\t{pw=}\t{letter=}\t{answer=}")
    return answer


def runA(input_data):
    data = input_data.split("\n")
    print(len(data), "entries\n")
    print("RESULT:", u.count(valid_password_a, data))


def runB(input_data):
    data = input_data.split("\n")
    print(len(data), "entries\n")
    print("RESULT:", u.count(valid_password_b, data))


if __name__ == "__main__":
    input_data = Puzzle(year=2020, day=2).input_data
    # u.run_for_inputs(TEST_INPUTS, runA, "A")
    u.run_for_inputs(TEST_INPUTS + [input_data], runA, "A")
    # u.run_for_inputs(TEST_INPUTS, runB, "B")
    u.run_for_inputs(TEST_INPUTS + [input_data], runB, "B")
