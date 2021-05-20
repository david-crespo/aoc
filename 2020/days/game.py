def parse_line(s):
    cmd, arg = s.split(" ")
    return (cmd, int(arg))


def parse_program(inp):
    return list(map(parse_line, inp.split("\n")))


class Game:
    def __init__(self, program):
        self.program = program[:]
        self.acc = 0
        self.i = 0
        self.executed = set()  # instruction indices

    def run(self):
        while not self.halted and self.i not in self.executed:
            self.executed.add(self.i)
            cmd, arg = self.program[self.i]
            if cmd == "acc":
                self.acc += arg
                self.i += 1
            elif cmd == "jmp":
                self.i += arg
            elif cmd == "nop":
                self.i += 1

    @property
    def halted(self):
        return self.i >= len(self.program)

    def halt(self):
        self.i = len(self.program)
