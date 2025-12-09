from aoc import read_input
from shapely import Polygon, box
from itertools import combinations, starmap, compress


def input():
    return read_input(splitter=["\n", ","], mapper=lambda x: tuple(map(int, x)))


def solve():
    data = input()
    rects = [
        (min(x1, x2), min(y1, y2), max(x1, x2), max(y1, y2))
        for (x1, y1), (x2, y2) in combinations(data, 2)
    ]
    areas = [(x2 - x1 + 1) * (y2 - y1 + 1) for (x1, y1, x2, y2) in rects]

    part1 = max(areas)
    poly = Polygon(data)
    part2 = max(compress(areas, map(poly.contains, starmap(box, rects))))
    return part1, part2


print(solve())
