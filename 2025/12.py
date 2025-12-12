from aoc import read_input


def solve():
    data = read_input(splitter=["\n\n"])
    print(data)
    regions = [
        (tuple(map(int, k.split("x"))), list(map(int, v.split())))
        for k, v in (r.split(": ") for r in data[-1].split("\n"))
    ]
    count = 0
    for region, values in regions:
        x, y = region
        count += x // 3 * y // 3 >= sum(values)
    return count


print(solve())
