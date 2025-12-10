from aoc import read_input
from itertools import combinations
from functools import reduce
from scipy.optimize import linprog
import operator


def parse_part1(splits):
    target, *schematics, _ = splits
    target = int(target[::-1].strip("[]").replace(".", "0").replace("#", "1"), 2)
    schematics = [
        sum(1 << b for b in bits)
        for bits in ([int(x) for x in s.strip("()").split(",")] for s in [*schematics])
    ]
    return (target, schematics)


def parse_part2(splits):
    _, *schematics, joltage = splits
    schematics = [[int(x) for x in s.strip("()").split(",")] for s in [*schematics]]
    joltage = [int(x) for x in joltage.strip("{}").split(",")]
    return (schematics, joltage)


def do_light_combo(target, schematics, presses):
    for combo in combinations(schematics, presses):
        if reduce(operator.ixor, combo) == target:
            return True
    return False


def solve1():
    data = read_input(splitter=["\n", " "], mapper=parse_part1)
    totals = []
    for target, schematics in data:
        presses = 1
        while not do_light_combo(target, schematics, presses):
            presses += 1
        totals.append(presses)
    return sum(totals)


def solve2():
    data = read_input(splitter=["\n", " "], mapper=parse_part2)
    totals = []
    for schematics, joltages in data:
        costs = [1] * len(schematics)
        eqs = [[i in b for b in schematics] for i in range(len(joltages))]
        totals.append(linprog(costs, A_eq=eqs, b_eq=joltages, integrality=1).fun)
    return sum(totals)


print(solve1())
print(solve2())
