from math import floor
import numpy as np

BASE_PATTERN = np.array([0, 1, 0, -1], dtype=np.int8)


def parse_input(in_str):
    return [int(d) for d in in_str]


# just need from start_idx to the next multiple of 6500


def run_phase(input_segment):
    output = []
    for digit in reversed(input_segment):
        prev = output[-1] if output else 0
        output.append((prev + digit) % 10)
    return list(reversed(output))


def print_code(input_segment, target_seg_idx):
    print("".join(map(str, input_segment[target_seg_idx : target_seg_idx + 8])))


def run_n_phases(raw_input, n=100):
    orig_input = parse_input(raw_input)

    input_segment = orig_input * 10  # min reps required to sum to zero

    target_idx = int(raw_input[:7])
    # target_idx = 319992
    target_seg_idx = target_idx % len(input_segment)

    if target_idx <= (len(orig_input) * 10000 / 2):
        raise Exception("target idx not far enough into second half")

    print_code(input_segment, target_seg_idx)
    for i in range(n):
        input_segment = run_phase(input_segment)
        print_code(input_segment, target_seg_idx)


if __name__ == "__main__":
    # test()

    with open("static/day16-input.txt") as f:
        raw_input = f.readline().strip()

    # raw_input = "03036732577212944063491565474664"
    raw_input = "02935109699940807407585447034323"
    # raw_input = "03081770884921959731165446850517"

    run_n_phases(raw_input)
