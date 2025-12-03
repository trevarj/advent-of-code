from aoc import read_input
from functools import partial


def parser(lines):
    return list(map(list, lines))


def find_max_and_rest(xs, rest_min=0):
    idx = 0
    max = xs[idx]
    for i, x in enumerate(xs):
        if x > max and len(xs) - i >= rest_min:
            max = x
            idx = i
    return max, xs[idx + 1 :]


def find_batteries(row, n=12, acc=""):
    if n == 0:
        return int(acc)
    max, rest = find_max_and_rest(row, n)
    return find_batteries(rest, n - 1, acc + str(max))


def solve(fn):
    return sum(map(fn, read_input(parser)))


def solve1():
    return solve(partial(find_batteries, n=2))


def solve2():
    return solve(find_batteries)


print(solve1())
print(solve2())
