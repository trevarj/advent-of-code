from aoc import read_input
from functools import reduce
import math
import itertools as it
import operator


def solve():
    data = read_input(
        splitter=["\n", ","],
        mapper=lambda x: tuple(map(int, x)),
    )
    connections = sorted(it.combinations(data, 2), key=lambda x: math.dist(*x))
    circuits = {node: {node} for pair in connections for node in pair}
    for i, (a, b) in enumerate(connections):
        for c in circuits:
            if a in circuits[c]:
                c1 = c
            if b in circuits[c]:
                c2 = c
        if c1 != c2:
            circuits[c1] |= circuits[c2]
            del circuits[c2]
        if i + 1 == 1000:
            yield reduce(
                operator.mul, sorted(map(len, [circuits[b] for b in circuits]))[-3:]
            )
        if len(circuits) == 1:
            yield a[0] * b[0]


g = solve()
print(next(g))
print(next(g))
