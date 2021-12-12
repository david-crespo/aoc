import os
from collections import defaultdict, Counter


sample_input = """
start-A
start-b
A-c
A-b
b-d
A-end
b-end
""".strip()

sample2 = """
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def parse(input):
    out = defaultdict(set)
    for line in input.split("\n"):
        [a, b] = line.split("-")
        if a != "end":
            out[a].add(b)
        if a != "start":
            out[b].add(a)
    return out


def get_paths_from(path_so_far, graph):
    last = path_so_far[-1]

    if last == "end":
        return set([path_so_far])

    out = graph[last]
    if all(n.islower() and n in path_so_far for n in out):
        return set()

    paths = set()
    for n in out:
        if n.isupper() or n not in path_so_far:
            paths |= get_paths_from(path_so_far + (n,), graph)
    return paths


def run(input):
    inp = parse(input)
    paths = get_paths_from(("start",), inp)
    print(len(paths))


def is_lower(n):
    return n.islower() and n != "end" and n != "start"


#     start
#     /   \
# c--A-----b--d
#     \   /
#      end


def get_paths_from2(path_so_far, graph):
    last = path_so_far[-1]
    if last == "end":
        return set([path_so_far])

    out = graph[last] - {"start"}
    lowers = Counter(n for n in path_so_far if is_lower(n))

    if any(v == 2 for v in lowers.values()):
        # took me 5 minutes to figure not to return set() here!
        out -= set(lowers.keys())

    paths = set()
    for n in out:
        paths |= get_paths_from2(path_so_far + (n,), graph)

    return paths


def run2(input):
    inp = parse(input)
    paths = get_paths_from2(("start",), inp)
    print(len(paths))


print("=" * 40)
run(sample_input)
run(sample2)
run(real_input)

print()
run2(sample_input)
run2(sample2)
run2(real_input)
