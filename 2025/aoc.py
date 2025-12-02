import inspect
import os
import re
from pathlib import Path


def int_parser(lines):
    """Split into lines and convert each line to a list of ints."""
    return [list(map(int, line.split())) for line in lines if line]


def read_input(parser=None, splitter="\n", input=None):
    """
    Automatically read the AoC input corresponding to the calling file.

    Parser can be a function that takes a list of strings (lines), or a regex
    string which will be matched upon and a tuple of the groups is returned.
    """
    if input is None:
        filename = inspect.stack()[1].filename
        caller_path = Path(filename).resolve()
        day = caller_path.stem

        input_path = Path("inputs") / f"{day}.txt"
        splits = input_path.read_text().rstrip("\n").split(splitter)
    else:
        splits = input.rstrip("\n").split(splitter)
    if callable(parser):
        return parser(splits)
    elif isinstance(parser, str):
        return [re.match(parser, line).groups() for line in splits]
    else:
        return splits


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
