import inspect
import os
import re
from pathlib import Path
from functools import partial


def int_parser(lines):
    """Split into lines and convert each line to a list of ints."""
    return [list(map(int, line.split())) for line in lines if line]


def re_parse(rx, line):
    gs = re.match(rx, line).groups()
    if gs:
        return gs
    else:
        return re.findall(rx, line)


def read_input(parser=None, splitter="\n", mapper=lambda x: x, input=None):
    """
    Automatically read the AoC input corresponding to the calling file.

    Parser can be a function that takes a list of strings (lines), or a regex
    string which will be matched upon and a tuple of the groups is returned.

    Mapper will be applied to each line after parsing.
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
        return list(map(mapper, parser(splits)))
    elif isinstance(parser, str):
        return list(map(partial(re_parse, parser), splits))
    else:
        return list(map(mapper, splits))


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
