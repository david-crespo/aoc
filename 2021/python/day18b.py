from __future__ import annotations
import os
import re
import math
from dataclasses import dataclass
from typing import Optional, Tuple

sample_input = """
[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
""".strip()

sample2 = """
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

sample3 = """
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
    return [parse(eval(line)) for line in input.split("\n")]


@dataclass
class Node:
    v: int | Tuple[Node, Node]
    parent: Optional[Node] = None

    def __repr__(self):
        if self.is_pair:
            return f"[{self.left},{self.right}]"
        return str(self.v)

    @property
    def is_pair(self):
        return isinstance(self.v, tuple)

    @property
    def left(self):
        return self.v[0] if isinstance(self.v, tuple) else None

    @property
    def right(self):
        return self.v[1] if isinstance(self.v, tuple) else None

    @property
    def pairs(self):
        if self.is_pair:
            return [self] + self.left.pairs + self.right.pairs
        return []

    @property
    def leaves(self):
        if self.is_pair:
            return self.left.leaves + self.right.leaves
        return [self]

    @property
    def is_4_deep(self):
        return (
            self.parent
            and self.parent.parent
            and self.parent.parent.parent
            and self.parent.parent.parent.parent
        )

    def replace_left(self, node):
        self.v = (node, self.right)

    def replace_right(self, node):
        self.v = (self.left, node)

    def split(self):
        a, b = math.floor(self.v / 2), math.ceil(self.v / 2)
        a, b = Node(a), Node(b)
        new_node = add(a, b, self.parent)
        if self is self.parent.left:
            self.parent.replace_left(new_node)
        else:
            self.parent.replace_right(new_node)


def parse(x):
    if isinstance(x, list):
        a, b = parse(x[0]), parse(x[1])
        return add(a, b)
    # otherwise it's a value
    return Node(x)


def add(a, b, parent=None):
    n = Node((a, b), parent)
    a.parent = n
    b.parent = n
    return n


def index_by_ref(lst, n):
    for i, x in enumerate(lst):
        if x is n:
            return i
    return None


def explode(n: Node):
    deep = [l for l in n.pairs if l.is_4_deep]
    if not deep:
        return False

    to_explode = deep[0]
    left_i = index_by_ref(n.leaves, to_explode.left)
    right_i = left_i + 1
    if left_i > 0:
        to_left = n.leaves[left_i - 1]
        to_left.v += to_explode.left.v
    if right_i < len(n.leaves) - 1:
        to_right = n.leaves[right_i + 1]
        to_right.v += to_explode.right.v

    parent = to_explode.parent
    new_node = Node(0, parent)
    if to_explode is parent.right:
        parent.replace_right(new_node)
    elif to_explode is parent.left:
        parent.replace_left(new_node)

    return True


def split(n: Node):
    big = [l for l in n.leaves if l.v >= 10]
    if big:
        big[0].split()
        return True
    return False


def red(n):
    changed = False
    while True:
        e = explode(n)
        if not e:
            s = split(n)
        if not e and not s:
            return n, changed
        changed = True


def step(x):
    if isinstance(x, list):
        if len(x) == 1:
            return x[0], True
        [a, b, *rest] = x
        return [add(red(a)[0], b), *rest], True

    return red(x)


def mag(n: Node):
    if n.is_pair:
        return 3 * mag(n.left) + 2 * mag(n.right)
    return n.v


def adds(xs):
    prev = xs
    while True:
        # print(prev)
        nxt, changed = step(prev)
        # print(nxt)
        if not changed:
            break
        prev = nxt
    # print(nxt)
    return mag(nxt)


def run(input):
    prev = parse_input(input)
    print(adds(prev))


def copy(n):
    return parse(eval(str(n)))


def run2(input):
    xs = parse_input(input)
    results = []
    for i in range(len(xs)):
        for j in range(len(xs)):
            if i != j:
                results.append(adds([copy(xs[i]), copy(xs[j])]))
    print(max(results))


print("=" * 40)
run(sample2)
run(real_input)

print()
run2(sample3)
run2(real_input)
