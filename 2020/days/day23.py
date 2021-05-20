import collections
import math
import os
import re
from functools import cache
from itertools import takewhile

from aocd.models import Puzzle
import util as u

DAY = int(os.path.basename(__file__)[3:5])


class NodeIter:
    def __init__(self, head):
        self.curr = head

    def __iter__(self):
        return self

    def __next__(self):
        if not self.curr:
            raise StopIteration
        else:
            curr = self.curr
            self.curr = self.curr.n
            return curr


class Node:
    def __init__(self, v):
        self.v = v
        self.n = None

    def __repr__(self):
        return f"({self.v}) -> {self.n and self.n.v}"

    def __str__(self):
        return str(self.v)

    def __iter__(self):
        return NodeIter(self)


def to_nodes(cups):
    first = Node(cups[0])
    nodes = {first.v: first}

    prev = first
    for c in cups[1:]:
        node = Node(c)
        nodes[c] = node
        prev.n = node
        prev = node
    node.n = first
    return first, nodes


def step(curr, nodes, N):
    t1 = curr.n
    t2 = t1.n
    t3 = t2.n
    curr.n = t3.n

    dv = curr.v - 1 if curr.v > 1 else N
    while dv == t1.v or dv == t2.v or dv == t3.v:
        dv = dv - 1 if dv > 1 else N

    dest = nodes[dv]

    right = dest.n
    dest.n = t1
    t3.n = right

    return curr.n


def run_a(inp):
    cups = [int(x) for x in inp]
    N = max(cups)
    curr, nodes = to_nodes(cups)

    for _ in range(100):
        curr = step(curr, nodes, N)

    print("".join(map(str, takewhile(lambda n: n.v != 1, nodes[1].n))))


def run_b(inp, N):
    cups = [int(x) for x in inp]
    top = max(cups)
    cups += list(range(top + 1, N + 1))
    curr, nodes = to_nodes(cups)

    for _ in range(N * 10):
        curr = step(curr, nodes, N)

    one = nodes[1]
    print(one.n.v * one.n.n.v)


if __name__ == "__main__":
    os.system("clear")
    sample = "389125467"
    inp = "712643589"

    print("=============== A ===============\n")
    run_a(sample)
    print("\n" + "-" * 15 + "\n")
    run_a(inp)

    print("\n=============== B ===============\n")
    run_b(sample, 1000000)
    print("\n" + "-" * 15 + "\n")
    run_b(inp, 1000000)
