from __future__ import annotations

from parse import parse
from typing import Optional, Tuple, List, Dict
from dataclasses import dataclass
from functools import cache


@dataclass
class Room:
    letter: str
    up: str | None
    down: str | None

    def can_take(self, letter):
        return letter == self.letter and (
            (not self.up and not self.down)
            or (self.down == self.letter and not self.up)
        )

    def pop(self) -> Room | None:
        if self.up and not self.complete:
            return self.up, Room(self.letter, None, self.down)
        elif not self.up and self.down and self.down != self.letter:
            return self.down, Room(self.letter, None, None)
        return None, None

    def push(self, letter) -> Room | None:
        if not self.up and not self.down:
            return Room(self.letter, None, letter)
        elif self.down == letter and not self.up:
            return Room(self.letter, letter, letter)
        return None

    @property
    def complete(self):
        return self.up == self.letter and self.down == self.letter

    def __hash__(self):
        return hash((self.letter, self.up, self.down))


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

        def u(letter):
            return self.rooms[letter].up or "."

        def d(letter):
            return self.rooms[letter].down or "."

        print("#############")
        print(f"#{h(0)}{h(1)}.{h(2)}.{h(3)}.{h(4)}.{h(5)}{h(6)}#")
        print(f"###{u('A')}#{u('B')}#{u('C')}#{u('D')}###")
        print(f"  #{d('A')}#{d('B')}#{d('C')}#{d('D')}#")
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
        # if x:
        #     print(i, x, s.rooms[x], s.rooms[x].can_take(x))
        if x and s.rooms[x].can_take(x) and way_clear(s, i):
            new_hallway = s.hallway.copy()
            new_hallway[i] = None
            new_rooms = s.rooms.copy()
            new_room = s.rooms[x].push(x)
            new_rooms[x] = new_room
            result.append(
                (State(new_hallway, new_rooms), energy(i, x, x, bool(new_room.up)))
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
                        energy(slot, x, popped, bool(r.up)),
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
def energy(i: int, room: str, letter: str, up: bool):
    step = 10 ** "ABCD".index(letter)
    bonus = 0 if up else 1
    return step * (steps[room][i] + bonus)


@cache
def min_energy(s0: State, energy: int):
    # print("^" * 20)
    # s0.print()
    # print("-" * 20)
    if s0.complete:
        return energy
    # for n, e in next_states(s0):
    #     n.print()

    ms = [m for s, e in next_states(s0) if (m := min_energy(s, e))]
    # if not ms:
    #     print("X" * 20)
    #     print("dead end")
    #     s0.print()
    #     print("-" * 20)
    #     for n, e in next_states(s0):
    #         n.print()
    # if ms:
    #     print(energy, min(ms))
    return energy + min(ms) if ms else None


def parse_input(input):
    lines = input.splitlines()
    l1 = parse("###{}#{}#{}#{}###", lines[2]).fixed
    l2 = parse("  #{}#{}#{}#{}#", lines[3]).fixed
    hallway = [None] * 7
    rooms = dict(
        A=Room("A", l1[0], l2[0]),
        B=Room("B", l1[1], l2[1]),
        C=Room("C", l1[2], l2[2]),
        D=Room("D", l1[3], l2[3]),
    )
    return State(hallway, rooms)


s1 = """
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
""".strip()

s2 = """
#############
#...........#
###B#A#D#C###
  #A#B#D#C#
  #########
""".strip()

real_input = """
#############
#...........#
###B#B#D#D###
  #C#A#A#C#
  #########
""".strip()


def run(input):
    inp = parse_input(input)
    print(min_energy(inp, 0))


def run2(input):
    inp = parse_input(input)


print("=" * 40)
run(s1)
run(real_input)

# print()
# run2(s1)
# run2(real_input)
