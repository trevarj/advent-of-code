from aoc import read_input
from collections import defaultdict
from functools import cache


def to_nodes(grid):
    d = defaultdict(lambda: None)
    for i, row in enumerate(grid):
        for j, ch in enumerate(row):
            d[(i, j)] = ch
    return d


def solve1():
    data = read_input(mapper=list)
    beams = {data[0].index("S")}
    splits = 0
    for line in data[1:]:
        for b in list(beams):
            if line[b] == "^":
                splits += 1
                beams.remove(b)
                beams.update([b + 1, b - 1])
    return splits


def solve2():
    data = read_input(mapper=list)
    start = (0, data[0].index("S"))
    nodes = to_nodes(data)

    @cache
    def split_timeline(b):
        x, y = b
        node = nodes[x, y]
        if node == "^":
            left = split_timeline((x + 1, y - 1))
            right = split_timeline((x + 1, y + 1))
            return left + right
        elif node == "." or node == "S":
            return split_timeline((x + 1, y))
        return 1

    return split_timeline(start)


print(solve1())
print(solve2())
