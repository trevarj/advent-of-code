from aoc import read_input
import itertools as it


def repeats_twice(s):
    mid = len(s) // 2
    return s[:mid] == s[mid:]


def repeats_n(s):
    chunks = (
        list(g)
        for k, g in it.groupby(
            s[i : i + j + 1] for j in range(len(s)) for i in range(0, len(s), j + 1)
        )
    )
    return any(
        x
        for x in chunks
        if len(x) >= 2 and len(set(x)) == 1 and len(x[0]) * len(x) == len(s)
    )


def detect_invalid(pair, repeater):
    start, end = map(int, pair)
    ids = (str(i) for i in range(start, end + 1))
    return list(map(int, filter(repeater, ids)))


def invalid_twice(pair):
    return detect_invalid(pair, repeats_twice)


def invalid_n(pair):
    return detect_invalid(pair, repeats_n)


def input():
    return read_input(r"(\d+)-(\d+)", ",")


def solve(invalid):
    return sum(sum(ids) for ids in map(invalid, input()))


def solve1():
    return solve(invalid_twice)


def solve2():
    return solve(invalid_n)


print(solve1())
print(solve2())
