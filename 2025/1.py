from aoc import read_input
from itertools import accumulate


def parse(lines):
    return [(1 if line[0] == "R" else -1) * int(line[1:]) for line in lines]


def rotate(acc, moves, limit=100):
    start, zeros = acc
    m = start + moves
    count, next = divmod(m, limit)
    count = abs(count)
    if next == 0:
        count += 1
    if moves > 0 and next == 0 or moves < 0 and start == 0:
        count -= 1
    return (next, zeros + count)


def solve():
    return accumulate(read_input(parse), rotate, initial=(50, 0))


def solve1():
    return [x[0] for x in solve()].count(0)


def solve2():
    return list(solve())[-1][1]


print(solve1())
print(solve2())
