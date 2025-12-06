from aoc import read_input
from functools import reduce
import operator

ops = {"+": operator.add, "*": operator.mul}


def solve(args):
    return reduce(ops[args[-1]], map(int, args[:-1]))


def solve1():
    data = read_input(splitter=["\n", " "])
    return sum(solve([n[col] for n in data]) for col in range(len(data[0])))


def solve2():
    data = read_input()
    reversed = [[ch for ch in line[::-1]] for line in data]
    op_list = [op for op in reversed[-1] if op != " "]
    aligned = ([n[col] for n in reversed[:-1]] for col in range(len(reversed[0])))
    problems = []
    curr = []
    for p in aligned:
        if all(n == " " for n in p):
            problems.append(curr)
            curr = []
            continue
        curr.append(int("".join(p)))
    problems.append(curr)
    total = 0
    for op, nums in zip(op_list, problems):
        total += reduce(ops[op], nums)
    return total


print(solve1())
print(solve2())
