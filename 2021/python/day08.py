sample_input = """
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
""".strip()

with open("input/day08.txt") as f:
    real_input = f.read().strip()


def parse(input):
    out = []
    for line in input.split("\n"):
        [a, b] = line.split(" | ")
        out.append((a.split(" "), b.split(" ")))
    return out


def run(input):
    inp = parse(input)
    sum = 0
    for digits, out in inp:
        sum += len([s for s in out if len(s) in {2, 3, 4, 7}])
    print(sum)


def eq(s1, s2):
    return s1 <= s2 and s1 >= s2


def decode(mapping, s):
    s_set = set(s)
    for i, d in mapping.items():
        if eq(d, s_set):
            return str(i)


def run2(input):
    inp = parse(input)
    sum = 0
    for digits, out in inp:
        m = {}
        m[1] = set(next(d for d in digits if len(d) == 2))
        m[4] = set(next(d for d in digits if len(d) == 4))
        m[7] = set(next(d for d in digits if len(d) == 3))
        m[8] = set(next(d for d in digits if len(d) == 7))

        top = m[7] - m[1]
        bottom_left = m[8] - m[4] - top

        two_five_three = [set(d) for d in digits if len(d) == 5]

        # two contains bottom left
        m[2] = next(d for d in two_five_three if bottom_left < d)

        five_three = [d for d in two_five_three if not eq(d, m[2])]

        m[9] = five_three[0] | five_three[1]

        middle = m[2] - m[1] - top - bottom_left
        m[0] = m[8] - middle

        if len(five_three[0] - m[7]) == 3:
            m[5] = five_three[0]
            m[3] = five_three[1]
        else:
            m[5] = five_three[1]
            m[3] = five_three[0]

        m[6] = (m[8] - m[1]) | m[5]

        sum += int("".join(decode(m, s) for s in out))

    print(sum)


print("=" * 40)
run(sample_input)
run(real_input)

print()
run2(sample_input)
run2(real_input)
