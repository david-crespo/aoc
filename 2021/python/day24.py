import os


def chunk_by(pred, xs):
    chunk = []
    for x in xs:
        if pred(x) and chunk:
            yield chunk
            chunk = []
        chunk.append(x)
    yield chunk


def chunk_to_func(i, lines):
    cmd, a = lines[0].split(" ")
    func = f"""
def c{i}(num, dat):
    {a} = num
    x, y, z = dat
"""
    for line in lines[1:]:
        cmd, a, b = line.split(" ")
        if cmd == "add":
            func += f"    {a} = {a} + {b}\n"
        elif cmd == "mul":
            func += f"    {a} = {a} * {b}\n"
        elif cmd == "div":
            func += f"    {a} = {a} // {b}\n"
        elif cmd == "mod":
            func += f"    {a} = {a} % {b}\n"
        elif cmd == "eql":
            func += f"    {a} = int({a} == {b})\n"

    func += "    return (x, y, z)\n"
    return func


def chunk_to_rust(i, lines):
    cmd, a = lines[0].split(" ")
    print(f"fn c{i}(num: i32, dat: &Dat) -> Dat {{")
    print("    let w = num;")
    print("    let (mut x, mut y, mut z) = dat;")
    for line in lines[1:]:
        cmd, a, b = line.split(" ")
        if cmd == "add":
            print(f"    {a} = {a} + {b};")
        elif cmd == "mul":
            print(f"    {a} = {a} * {b};")
        elif cmd == "div":
            print(f"    {a} = {a} / {b};")
        elif cmd == "mod":
            print(f"    {a} = {a} % {b};")
        elif cmd == "eql":
            print(f"    {a} = ({a} == {b}) as i32;")

    print("    (x, y, z)")
    print("}\n")


def input_to_python(input: str):
    chunks = chunk_by(lambda line: line.startswith("inp"), input.splitlines())
    chunks = [chunk_to_func(i, c) for i, c in enumerate(chunks)]
    names = ", ".join(f"c{i}" for i in range(len(chunks)))
    chunks.append(f"chunks = [{names}]")
    return "\n\n".join(chunks)


def input_to_rust(input: str):
    chunks = chunk_by(lambda line: line.startswith("inp"), input.splitlines())
    for i, c in enumerate(chunks):
        chunk_to_rust(i, c)


DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()


chunks = []  # real defn comes from exec, this is to make linter shut up
input_python = input_to_python(real_input)
# print(input_python)
exec(input_python)

# input_to_rust(real_input)


def run():
    dats = dict()
    dats[(0, 0, 0)] = 0
    for i, chunk in enumerate(chunks):
        new_dats = dict()
        # realized that w gets thrown out immediately every time, so
        # it's irrelevant. this reduces the number of unique dats
        for dat, high in dats.items():
            base = high * 10
            for d in range(1, 10):
                new_dat = chunk(d, dat)
                n = base + d
                # condition for part 1:
                if new_dats.get(new_dat, 0) < n:
                    # condition for part 2:
                    # if new_dat not in new_dats or new_dats[new_dat] > n:
                    new_dats[new_dat] = n
        dats = new_dats
        print(i, len(dats))
    valid = [(k, v) for k, v in dats.items() if k[2] == 0]
    print(valid)
    print("valid count:", len(valid))
    print("max:", max(v for k, v in valid))  # part 1
    print("min:", min(v for k, v in valid))  # part 2


print("=" * 40)
run()
