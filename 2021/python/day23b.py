from __future__ import annotations

from parse import parse
from typing import Optional, Tuple, List, Dict
from dataclasses import dataclass
from functools import cache


@dataclass
class Room:
    letter: str
    stack: List[str]

    def can_take(self, letter):
        return letter == self.letter and len(self.stack) < 4 and self.all_match

    def pop(self) -> Room | None:
        if self.stack and not self.complete:
            *rest, top = self.stack
            if not self.all_match:
                return top, Room(self.letter, rest)
        return None, None

    def push(self, letter) -> Room | None:
        if not self.complete:
            return Room(self.letter, self.stack + [letter])
        return None

    @property
    def all_match(self):
        return all(l == self.letter for l in self.stack)

    @property
    def complete(self):
        return len(self.stack) == 4 and self.all_match

    def __hash__(self):
        return hash((self.letter, tuple(self.stack)))


@dataclass
class State:
    hallway: List[Optional[str]]
    rooms: Dict[str, Room]

    @property
    def complete(self):
        return all(r.complete for r in self.rooms.values())

    def __hash__(self):
        return hash((tuple(self.hallway), tuple(self.rooms.values())))

    def print(self):
        def h(i):
            return self.hallway[i] or "."

        def r(letter, i):
            stack = self.rooms[letter].stack
            stack = stack + [None] * (4 - len(stack))
            return stack[i] or "."

        print("#############")
        print(f"#{h(0)}{h(1)}.{h(2)}.{h(3)}.{h(4)}.{h(5)}{h(6)}#")
        print(f"###{r('A',3)}#{r('B',3)}#{r('C',3)}#{r('D',3)}###")
        print(f"  #{r('A',2)}#{r('B',2)}#{r('C',2)}#{r('D',2)}#")
        print(f"  #{r('A',1)}#{r('B',1)}#{r('C',1)}#{r('D',1)}#")
        print(f"  #{r('A',0)}#{r('B',0)}#{r('C',0)}#{r('D',0)}#")
        print("  #########")
        print()


divs = dict(A=2, B=3, C=4, D=5)


@cache
def available_hallway_slots(s: State, room_letter: str):
    d = divs[room_letter]
    left, right = s.hallway[0:d], s.hallway[d:7]
    result = []
    for i, x in enumerate(left[::-1]):
        if x:
            break
        result.append(d - i - 1)

    for i, x in enumerate(right):
        if x:
            break
        result.append(d + i)

    # print(result)
    return result


@cache
def way_clear(s: State, i: int):
    letter = s.hallway[i]
    d = divs[letter]
    in_way = s.hallway[i + 1 : d] if i < d else s.hallway[d:i]
    return not any(in_way)


@cache
def next_states(s: State):
    result = []
    for i, x in enumerate(s.hallway):
        if x and s.rooms[x].can_take(x) and way_clear(s, i):
            new_hallway = s.hallway.copy()
            new_hallway[i] = None
            new_rooms = s.rooms.copy()
            new_room = s.rooms[x].push(x)
            new_rooms[x] = new_room
            result.append(
                (
                    State(new_hallway, new_rooms),
                    energy(i, x, x, 3 - len(s.rooms[x].stack)),
                )
            )

    for x, r in s.rooms.items():
        popped, new_r = r.pop()
        # print(x, r, popped, new_r)
        if new_r:
            new_rooms = s.rooms.copy()
            new_rooms[x] = new_r
            for slot in available_hallway_slots(s, x):
                new_hallway = s.hallway.copy()
                new_hallway[slot] = popped
                result.append(
                    (
                        State(new_hallway, new_rooms.copy()),
                        energy(slot, x, popped, 4 - len(r.stack)),
                    )
                )
    return result


steps = dict(
    A=[3, 2, 2, 4, 6, 8, 9],
    B=[5, 4, 2, 2, 4, 6, 7],
)
steps["C"] = steps["B"][::-1]
steps["D"] = steps["A"][::-1]


@cache
def energy(i: int, room: str, letter: str, extra: int):
    step = 10 ** "ABCD".index(letter)
    return step * (steps[room][i] + extra)


@cache
def min_energy(s0: State, energy: int):
    if s0.complete:
        return energy

    ms = [m for s, e in next_states(s0) if (m := min_energy(s, e))]
    return energy + min(ms) if ms else None


def parse_input(input):
    lines = input.splitlines()
    l1 = parse("###{}#{}#{}#{}###", lines[2]).fixed
    l2 = parse("  #{}#{}#{}#{}#", lines[3]).fixed
    l3 = parse("  #{}#{}#{}#{}#", lines[4]).fixed
    l4 = parse("  #{}#{}#{}#{}#", lines[5]).fixed
    hallway = [None] * 7
    rooms = list(zip(l1, l2, l3, l4))
    rooms = dict(
        A=Room("A", list(rooms[0])[::-1]),
        B=Room("B", list(rooms[1])[::-1]),
        C=Room("C", list(rooms[2])[::-1]),
        D=Room("D", list(rooms[3])[::-1]),
    )
    return State(hallway, rooms)


s1 = """
#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########
""".strip()

s2 = """
#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########
""".strip()

real_input = """
#############
#...........#
###B#B#D#D###
  #D#C#B#A#
  #D#B#A#C#
  #C#A#A#C#
  #########
""".strip()


def mr(s: State, i: int, r: str):
    s.hallway[i] = None
    s.rooms[r].stack.append(r)
    s.print()


def mh(s: State, i: int, r: str):
    x = s.rooms[r].stack.pop()
    s.hallway[i] = x
    s.print()


def run(input):
    inp = parse_input(input)
    inp.print()
    print(min_energy(inp, 0))
    print()


print("=" * 40)
run(s1)
run(real_input)
