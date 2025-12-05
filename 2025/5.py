from aoc import read_input


def merge_ranges(ranges):
    merged = []
    for current in ranges:
        if not merged or merged[-1][1] < current[0]:
            merged.append(current)
        else:
            merged[-1][1] = max(merged[-1][1], current[1])

    return merged


def input():
    return read_input(splitter=["\n\n", "\n", "-"])


def solve1():
    ranges, ids = input()
    ranges = [list(map(int, r)) for r in ranges]
    ids = [int(i) for i in ids]
    valid = 0
    for id in ids:
        for lo, hi in ranges:
            if id >= lo and id <= hi:
                valid += 1
                break
    return valid


def solve2():
    ranges, _ = input()
    merged = merge_ranges(sorted(list(map(int, r)) for r in ranges))
    valid = 0
    for lo, hi in merged:
        valid += hi - lo + 1
    return valid


print(solve1())
print(solve2())
