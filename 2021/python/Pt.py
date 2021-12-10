def add(p1, p2):
    return (p1[0] + p2[0], p1[1] + p2[1])


def scale(p, s):
    return (s * p[0], s * p[1])


def path(start, dir, length):
    return [add(start, scale(dir, i)) for i in range(length)]
