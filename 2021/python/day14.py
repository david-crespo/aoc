import os
from collections import Counter, defaultdict

sample_input = """
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def parse_input(input):
    start, rules = input.split("\n\n")
    return (start, dict((r.split(" -> ")) for r in rules.split("\n")))


def run(input):
    start, rules = parse_input(input)
    for i in range(10):
        for a, b in rules.items():
            start = start.replace(a, a[0] + b.lower() + a[1])
            # second one to handle overlapping pairs took me a good 5m to figure out
            start = start.replace(a, a[0] + b.lower() + a[1])
        start = start.upper()
    counts = [v for _, v in Counter(start).most_common()]
    print(counts[0] - counts[-1])


def letter_counts(pc):
    counts = defaultdict(int)
    for k, v in pc.items():
        counts[k[0]] += v / 2
        counts[k[1]] += v / 2
    return Counter(counts)


def run2(input):
    start, rules = parse_input(input)
    pairs = Counter("".join(p) for p in zip(start, start[1:]))
    for i in range(40):
        new_pairs = defaultdict(int)
        for a, b in rules.items():
            p1, p2 = (a[0] + b, b + a[1])
            count = pairs[a]
            if count > 0:
                del pairs[a]
                new_pairs[p1] += count
                new_pairs[p2] += count
        pairs.update(new_pairs)
    counts = [v for _, v in letter_counts(pairs).most_common()]
    # gives a 0.5 so I had to guess at random whether it was a round down or up lol
    print(counts[0] - counts[-1])


print("=" * 40)
run(sample_input)
run(real_input)

print()
run2(sample_input)
run2(real_input)
