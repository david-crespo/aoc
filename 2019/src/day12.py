from dataclasses import dataclass
from collections import namedtuple

import cProfile


@dataclass(unsafe_hash=True)
class Moon:
    x: int
    y: int
    z: int
    vx: int
    vy: int
    vz: int

    def tuple(self):
        return (self.x, self.y, self.z, self.vx, self.vy, self.vz)


def compare(a, b):
    return 1 if b > a else -1 if b < a else 0


def tick(moons, pairs):
    # gravity
    for m1, m2 in pairs:
        # m1.vx += 1 if m2.x > m1.x else -1 if m2.x < m1.x else 0
        # m1.vy += 1 if m2.y > m1.y else -1 if m2.y < m1.y else 0
        # m1.vz += 1 if m2.z > m1.z else -1 if m2.z < m1.z else 0
        m1.vx += compare(m1.x, m2.x)
        m1.vy += compare(m1.y, m2.y)
        m1.vz += compare(m1.z, m2.z)

    # velocity
    for m in moons:
        m.x += m.vx
        m.y += m.vy
        m.z += m.vz

    return moons


def moon_energy(m):
    return (abs(m.x) + abs(m.y) + abs(m.z)) * (abs(m.vx) + abs(m.vy) + abs(m.vz))


def energy(moons):
    return sum(moon_energy(m) for m in moons)


def energy_after_ticks(moons, n):
    pairs = get_pairs(moons)
    for _ in range(n):
        tick(moons, pairs)
    print(energy(moons))


def ticks_to_repeat(moons):
    pairs = get_pairs(moons)
    seen = set(tuple(m.tuple() for m in moons))
    count = 0
    while True and count < 300000:
        tick(moons, pairs)
        if count % 100000 == 0:
            print(count)

        tup = tuple(m.tuple() for m in moons)
        if tup in seen:
            return count
        else:
            seen.add(tup)

        count += 1
    return count


def get_pairs(moons):
    pairs = []
    for i, m1 in enumerate(moons):
        for j, m2 in enumerate(moons):
            if i == j:
                continue
            pairs.append((m1, m2))
    return pairs


def main():
    moons = [
        Moon(x=13, y=-13, z=-2, vx=0, vy=0, vz=0),
        Moon(x=16, y=2, z=-15, vx=0, vy=0, vz=0),
        Moon(x=7, y=-18, z=-12, vx=0, vy=0, vz=0),
        Moon(x=-3, y=-8, z=-8, vx=0, vy=0, vz=0),
    ]
    test_moons = [
        Moon(x=-1, y=0, z=2, vx=0, vy=0, vz=0),
        Moon(x=2, y=-10, z=-7, vx=0, vy=0, vz=0),
        Moon(x=4, y=-8, z=8, vx=0, vy=0, vz=0),
        Moon(x=3, y=5, z=-1, vx=0, vy=0, vz=0),
    ]
    test_moons2 = [
        Moon(x=-8, y=-10, z=0, vx=0, vy=0, vz=0),
        Moon(x=5, y=5, z=10, vx=0, vy=0, vz=0),
        Moon(x=2, y=-7, z=3, vx=0, vy=0, vz=0),
        Moon(x=9, y=-8, z=-3, vx=0, vy=0, vz=0),
    ]

    # energy_after_ticks(moons, 1000)
    print(ticks_to_repeat(moons))


if __name__ == "__main__":
    main()
    # cProfile.run("main()")

