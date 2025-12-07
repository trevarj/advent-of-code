from aoc import read_input
from collections import defaultdict
from functools import cache


def to_nodes(grid):
    d = defaultdict(lambda: None)
    for i, row in enumerate(grid):
        for j, ch in enumerate(row):
            d[(i, j)] = ch
    return d


def solve():
    data = read_input(mapper=list)
    start = (0, data[0].index("S"))
    nodes = to_nodes(data)
    splits = 0

    @cache
    def split_timeline(b):
        x, y = b
        node = nodes[x, y]
        if node == "^":
            nonlocal splits
            splits += 1
            left = split_timeline((x + 1, y - 1))
            right = split_timeline((x + 1, y + 1))
            return left + right
        elif node == "." or node == "S":
            return split_timeline((x + 1, y))
        return 1

    part2 = split_timeline(start)
    return splits, part2


print(solve())
