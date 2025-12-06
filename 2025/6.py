from aoc import read_input
from functools import reduce
import operator

ops = {"+": operator.add, "*": operator.mul}


def solve(acc, args):
    return acc + reduce(ops[args[-1]], map(int, args[:-1]))


def solve1():
    data = read_input(splitter=["\n", " "])
    return reduce(solve, ([n[col] for n in data] for col in range(len(data[0]))), 0)


def solve2():
    data = read_input()
    op_list = [op for op in data[-1] if op != " "]
    aligned = (([n[col] for n in data[:-1]]) for col in range(len(data[0])))
    problems = []
    curr = []
    for p in aligned:
        if all(n == " " for n in p):
            problems.append(curr + [op_list.pop(0)])
            curr = []
            continue
        curr.append("".join(p))
    problems.append(curr + [op_list.pop(0)])
    return reduce(solve, problems, 0)


print(solve1())
print(solve2())
