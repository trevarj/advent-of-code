from aoc import read_input
from collections import defaultdict
import time


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


def is_free(pos, ns, stage):
    return (
        sum(1 for n in adjacent(pos, ns) if n == "@" or (stage == 1 and n == "x")) < 4
    )


def solve():
    grid = read_input(mapper=list)
    ns = nodes(grid)
    accessible = None
    count = 0
    removed = -1
    stage = 1
    while removed != 0:
        removed = 0
        for k, v in list(ns.items()):
            if v == "@" and is_free(k, ns, stage):
                ns[k] = "x"
                count += 1
                removed += 1
        stage += 1
        if accessible is None:
            accessible = removed

    return accessible, count


t = time.time()
p1, p2 = solve()
end = time.time()
print(p1)
print(p2)
print(f"elapsed time {end - t}")
