import os
import re
import math

sample_input = """
[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
""".strip()

sample_input = """
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
""".strip()

sample_input = """
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
""".strip()

# sample_input = """
# [[[[4,3],4],4],[7,[[8,4],9]]]
# [1,1]
# """.strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def parse_input(input):
    return input.split("\n")


def add(a, b):
    return f"[{a},{b}]"


def find_exp(x):
    ct = 0
    for i in range(len(x)):
        c = x[i]
        if ct == 4 and c == "[":
            return i
        if c == "[":
            ct += 1
        elif c == "]":
            ct -= 1
    return None


def find_fwd(s, init, pred):
    result = []
    for i, c in enumerate(s[init + 1 :]):
        if pred(c):
            result.append(init + i + 1)
        if not pred(c) and result:
            return sorted(result)
    return result if result else None


def find_back(s, init, pred):
    result = []
    for i, c in enumerate(s[init - 1 :: -1]):
        if pred(c):
            result.append(init - i - 1)
        if not pred(c) and result:
            return sorted(result)
    return None


def is_num(s):
    return s.isnumeric()


def num(s, idxs):
    return int("".join(s[i] for i in idxs))


def explode(x):
    left_br = find_exp(x)
    if left_br is None:
        return x

    left = find_fwd(x, left_br, is_num)
    right = find_fwd(x, left[-1], is_num)

    right_br = find_fwd(x, left_br, lambda c: c == "]")[0]
    # print(left_br, left, right, right_br)

    to_left = find_back(x, left_br, is_num)
    to_right = find_fwd(x, right_br, is_num)
    # print(to_left, to_right)

    if to_left:
        to_left_val = num(x, to_left) + num(x, left)
        new_x = x[: to_left[0]] + str(to_left_val) + x[to_left[-1] + 1 : left_br]
        # print(to_left_val)
    else:
        new_x = x[:left_br]

    new_x += "0"

    if to_right:
        to_right_val = num(x, to_right) + num(x, right)
        new_x += (
            x[right_br + 1 : to_right[0]] + str(to_right_val) + x[to_right[-1] + 1 :]
        )
        # print(to_right_val)
    else:
        new_x += x[right_br + 1 :]

    return new_x


def split(x):
    nums = [int(s) for s in re.findall(r"\d+", x)]
    big = [n for n in nums if n >= 10]
    if big:
        v = big[0]
        i = x.find(str(v))
        a, b = math.floor(v / 2), math.ceil(v / 2)
        return x[:i] + f"[{a},{b}]" + x[i + 2 :]
    return x


def red(x):
    prev = x
    while True:
        nxt = explode(prev)
        if nxt == prev:
            nxt = split(prev)
        if nxt == prev:
            return nxt
        prev = nxt


def step(x):
    if isinstance(x, list):
        if len(x) == 1:
            return x[0]
        [a, b, *rest] = x
        return [add(red(a), b), *rest]

    return red(x)


def mag(x):
    v = eval(x) if isinstance(x, str) else x
    if isinstance(v, list):
        return 3 * mag(v[0]) + 2 * mag(v[1])
    return v


def adds(xs):
    prev = xs
    while True:
        nxt = step(prev)
        # print(nxt)
        if nxt == prev:
            break
        prev = nxt
    return mag(nxt)


def run(input):
    prev = parse_input(input)
    print(adds(prev))


def run2(input):
    xs = parse_input(input)
    results = []
    for i in range(len(xs)):
        for j in range(len(xs)):
            if i != j:
                results.append(adds([xs[i], xs[j]]))
    print(max(results))


print("=" * 40)
# run(sample_input)
run(real_input)

print()
run2(sample_input)
run2(real_input)
