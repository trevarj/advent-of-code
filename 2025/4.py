from aoc import read_input
from collections import defaultdict


def adjacent(pos, nodes):
    x, y = pos
    return [
        nodes[(x - 1, y - 1)],
        nodes[(x - 1, y)],
        nodes[(x - 1, y + 1)],
        nodes[(x, y - 1)],
        nodes[(x, y + 1)],
        nodes[(x + 1, y - 1)],
        nodes[(x + 1, y)],
        nodes[(x + 1, y + 1)],
    ]


def nodes(grid):
    d = defaultdict(lambda: None)
    for i, row in enumerate(grid):
        for j, ch in enumerate(row):
            d[(i, j)] = ch
    return d


def solve():
    grid = read_input(mapper=list)
    ns = nodes(grid)
    accessible = None
    count = 0
    removeable = set([1])
    while removeable:
        removeable = set()
        for k, v in list(ns.items()):
            if v == "@" and adjacent(k, ns).count("@") < 4:
                removeable.add(k)
                count += 1
        if accessible is None:
            accessible = count
        ns = defaultdict(
            lambda: None, {k: v for k, v in ns.items() if k not in removeable}
        )

    return accessible, count


p1, p2 = solve()
print(p1)
print(p2)
