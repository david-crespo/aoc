import os
from collections import namedtuple
import math

sample_input = """
D2FE28
""".strip()

sample2 = "38006F45291200"
sample3 = "EE00D40C823060"

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


P = namedtuple("P", ["v", "t", "sub"])


def chunks(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i : i + n]


def pad_to_mult(b, n):
    l = len(b)
    pad = math.ceil(l / n) * n - l
    return pad * "0" + b


def parse_input(input):
    data = bin(int(input, 16))[2:]
    data = pad_to_mult(data, 8)
    return data


def parse(inp):
    version = int(inp[:3], 2)
    type_id = int(inp[3:6], 2)
    data = inp[6:]
    if type_id == 4:
        s = ""
        cs = 0
        for c in chunks(data, 5):
            cs += 1
            s += c[1:]
            if c[0] == "0":
                break
        rest = data[cs * 5 :]
        return P(version, type_id, int(s, 2)), rest
    else:
        if data[0] == "1":
            num_sp = int(data[1:12], 2)
            rest = data[12:]
            subs = []
            while rest and len(subs) < num_sp:
                sub, rest = parse(rest)
                subs.append(sub)
        else:
            sp_len = int(data[1:16], 2)
            rest = data[16:]
            orig_rest_len = len(rest)
            subs = []
            while rest and len(rest) > orig_rest_len - sp_len:
                sub, rest = parse(rest)
                subs.append(sub)

        return P(version, type_id, subs), rest


def add_versions(p):
    return p.v + (
        sum(add_versions(sp) for sp in p.sub) if isinstance(p.sub, list) else 0
    )


def run(input):
    inp = parse_input(input)
    result = parse(inp)[0]
    # print(result)
    print(add_versions(result))


def calc(p):
    if p.t == 0:
        return sum(calc(s) for s in p.sub)
    elif p.t == 1:
        return math.prod(calc(s) for s in p.sub)
    elif p.t == 2:
        return min(calc(s) for s in p.sub)
    elif p.t == 3:
        return max(calc(s) for s in p.sub)
    elif p.t == 4:
        return p.sub
    elif p.t == 5:
        a, b = p.sub
        return int(calc(a) > calc(b))
    elif p.t == 6:
        a, b = p.sub
        return int(calc(a) < calc(b))
    elif p.t == 7:
        a, b = p.sub
        return int(calc(a) == calc(b))


def run2(input):
    inp = parse_input(input)
    result = parse(inp)[0]
    # print(result)
    print(calc(result))


print("=" * 40)
run("D2FE28")
run("EE00D40C823060")
run("8A004A801A8002F478")
run("620080001611562C8802118E34")
run("C0015000016115A2E0802F182340")
run("A0016C880162017C3686B18A3D4780")
run(real_input)

print()
run2("D2FE28")
run2("C200B40A82")
run2("04005AC33890")
run2("880086C3E88112")
run2("CE00C43D881120")
run2("D8005AC2A8F0")
run2("F600BC2D8F")
run2("9C005AC2F8F0")
run2("9C0141080250320F1802104A08")
run2(real_input)
