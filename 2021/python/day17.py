import os
from parse import parse


sample_input = """
target area: x=20..30, y=-10..-5
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def parse_input(input):
    return parse("target area: x={:d}..{:d}, y={:d}..{:d}", input).fixed


def traj(v):
    x, y = (0, 0)
    vx, vy = v
    while True:
        x += vx
        y += vy
        yield (x, y)
        vx = max(0, vx - 1)
        vy -= 1


def hits(v, target):
    # print(v, target)
    max_y = 0
    for pt in traj(v):
        x, y = pt
        max_y = max(max_y, y)
        x1, x2, y1, y2 = target
        if x1 <= x <= x2 and y1 <= y <= y2:
            return True, max_y
        # had y2 instead of y1, cost me like 500 rank
        if x > x2 or y < y1:
            return False, None


def run(input):
    target = parse_input(input)
    print(target)
    hit_vs = []
    for x in range(1, 300):
        for y in range(-200, 200):
            v = (x, y)
            h, max_y = hits(v, target)
            if h:
                hit_vs.append((v, max_y))
    print("part 1:", max(max_y for v, max_y in hit_vs))
    print("part 2:", len(hit_vs))


print("=" * 40)
run(sample_input)
print()
run(real_input)
