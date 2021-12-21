from functools import cache


sample_input = (4, 8)
real_input = (4, 6)


def run(inp):
    p = [s - 1 for s in inp]
    s = [0, 0]
    rolls = 0
    ri = 1
    pi = 0
    while s[0] < 1000 and s[1] < 1000:
        roll = ri * 3 + 3
        p[pi] = (p[pi] + roll) % 10
        s[pi] += p[pi] + 1  # 0-9 becomes 1-10
        # print(pi, ri, p[pi] + 1, s)
        ri += 3
        pi = (pi + 1) % 2
        rolls += 1

    print(min(s) * rolls * 3)


def sumt(*ps):
    return tuple(map(sum, zip(*ps)))


@cache
def get_wins(p1, s1, p2, s2, pi, rolls):
    if len(rolls) == 3:
        roll = sum(rolls)
        if pi == 0:
            p1 = (p1 + roll) % 10
            s1 += p1 + 1  # 0-9 becomes 1-10
            if s1 >= 21:
                return (1, 0)
        else:
            p2 = (p2 + roll) % 10
            s2 += p2 + 1  # 0-9 becomes 1-10
            if s2 >= 21:
                return (0, 1)
        rolls = ()
    args = (p1, s1, p2, s2, (pi + 1) % 2)
    return sumt(
        get_wins(*args, rolls + (1,)),
        get_wins(*args, rolls + (2,)),
        get_wins(*args, rolls + (3,)),
    )


def run2(inp):
    p1, p2 = [s - 1 for s in inp]
    args = (p1, 0, p2, 0, 0)
    result = sumt(
        get_wins(*args, (1,)),
        get_wins(*args, (2,)),
        get_wins(*args, (3,)),
    )
    print(result)


print("=" * 40)
run(sample_input)
run(real_input)

print()
run2(sample_input)
run2(real_input)
