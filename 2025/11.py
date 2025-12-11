from aoc import read_input
from functools import cache


def solve():
    data = {k[:-1]: v for k, *v in read_input(mapper=str.split)}

    @cache
    def flow(node, fft, dac):
        match node:
            case "out":
                return fft and dac
            case "fft":
                fft = True
            case "dac":
                dac = True
        return sum(flow(next, fft, dac) for next in data[node])

    return flow("you", True, True), flow("svr", False, False)


print(solve())
