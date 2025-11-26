import inspect
import os
from pathlib import Path


def int_parser(lines):
    """Split into lines and convert each line to a list of ints."""
    return [list(map(int, line.split())) for line in lines if line]


def read_input(parser=int_parser):
    """
    Automatically read the AoC input corresponding to the calling file.
    """
    filename = inspect.stack()[1].filename
    caller_path = Path(filename).resolve()
    day = caller_path.stem

    input_path = Path("inputs") / f"{day}.txt"
    lines = input_path.read_text().rstrip("\n").splitlines()
    return parser(lines)


def generate_days(days=12):
    template = """from aoc import read_input


def solve1():
    data = read_input()
    pass


def solve2():
    data = read_input()
    pass


print(solve1())
print(solve2())
"""

    for day in range(days):
        file = f"{day+1}.py"
        if os.path.exists(file):
            continue
        with open(file, "w") as file:
            file.write(template)
