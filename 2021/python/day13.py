import os
import util as u

sample_input = """
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def parse_fold(s):
    a, b = s.lstrip("fold along ").split("=")
    return (a, int(b))


def pp(pts):
    max_x = max(x for x, y in pts)
    max_y = max(y for x, y in pts)
    for y in range(max_y + 1):
        print("".join("#" if (x, y) in pts else "." for x in range(max_x + 1)))
    print()


def parse(input):
    pts, folds = input.split("\n\n")
    return (
        set(tuple(map(int, pt.split(","))) for pt in pts.split("\n")),
        [parse_fold(f) for f in folds.split("\n")],
    )


def run(input):
    pts, folds = parse(input)
    # part 1 uses only folds[:1]
    for axis, n in folds:
        # pp(pts)
        if axis == "x":
            left = set((x, y) for x, y in pts if x < n)
            right = set((2 * n - x, y) for x, y in pts if x > n)
            pts = left | right
        else:
            above = set((x, y) for x, y in pts if y < n)
            below = set((x, 2 * n - y) for x, y in pts if y > n)
            pts = above | below

    pp(pts)
    print(len(pts))


def run2(input):
    inp = parse(input)


print("=" * 40)
run(sample_input)
run(real_input)
