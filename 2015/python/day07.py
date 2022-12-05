import os

from parse import parse


s1 = """
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
""".strip()

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


def pint(s: str) -> str | int:
    return int(s) if s.isnumeric() else s


def parse_input(input):
    out = dict()
    for line in input.split("\n"):
        if "AND" in line or "OR" in line or "SHIFT" in line:
            a, op, b, result = parse("{} {} {} -> {}", line).fixed
            out[result] = (op, pint(a), pint(b))
        elif "NOT" in line:
            op, a, result = parse("{} {} -> {}", line).fixed
            out[result] = (op, pint(a))
        else:
            a, result = parse("{} -> {}", line).fixed
            out[result] = pint(a)
    return out


def exec(prog, cmd: str) -> int:
    if isinstance(cmd, int):
        return cmd

    expr = prog[cmd]
    if isinstance(expr, int):
        return expr
    elif isinstance(expr, str):
        result = exec(prog, expr)
    else:
        op, *args = expr
        if op == "AND":
            a, b = args
            result = exec(prog, a) & exec(prog, b)
        elif op == "OR":
            a, b = args
            result = exec(prog, a) | exec(prog, b)
        elif op == "LSHIFT":
            a, b = args
            result = exec(prog, a) << exec(prog, b)
        elif op == "RSHIFT":
            a, b = args
            result = exec(prog, a) >> exec(prog, b)
        else:  # op == "NOT"
            a = args[0]
            comp = ~exec(prog, a)
            result = comp if comp >= 0 else 65535 + comp + 1

    # memoization makes it instant
    prog[cmd] = result
    return result


def run(input):
    prog = parse_input(input)
    # print(prog)
    print(f"a: {exec(prog, 'a')}")


def run2(input):
    prog = parse_input(input)
    a = exec(prog, "a")
    prog = parse_input(input)
    prog["b"] = a
    print(f"a: {exec(prog, 'a')}")


print("=" * 40)
# run(s1)
run(real_input)

print()
# run2(s1)
run2(real_input)
