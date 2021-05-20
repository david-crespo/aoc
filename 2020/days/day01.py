from aocd.models import Puzzle
import util as u

TEST_INPUTS = [
    """
1721
979
366
299
675
1456
"""
]
TEST_INPUTS = [t.strip() for t in TEST_INPUTS]


def run(input_data):
    data = [int(x) for x in input_data.split("\n")]
    print(len(data), "entries")
    for i, x in enumerate(data):
        for j, y in enumerate(data):
            # first star was only two loops
            for k, z in enumerate(data):
                if i < j and j < k and x + y + z == 2020:
                    print(x, y, z, x * y * z)
    print("-" * 20)


if __name__ == "__main__":
    for t in TEST_INPUTS:
        run(t)

    input_data = Puzzle(year=2020, day=1).input_data
    run(input_data)
