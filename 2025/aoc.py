import inspect
import os
import re
from pathlib import Path
from functools import partial


def int_parser(lines):
    """Split into lines and convert each line to a list of ints."""
    return [list(map(int, line.split())) for line in lines if line]


def re_parse(rx, line):
    matched = re.match(rx, line)
    if matched and matched.groups():
        return matched.groups()
    else:
        return re.findall(rx, line)


def split_rec(s, splitters):
    if not splitters:
        return s
    splitter = splitters[0]
    parts = [p for p in s.split(splitter) if p]
    if len(parts) == 1:
        return s
    return [split_rec(p, splitters[1:]) for p in parts]


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
        text = input_path.read_text().rstrip("\n")
    else:
        text = input.rstrip("\n")

    if isinstance(splitter, list):
        splits = split_rec(text, splitter)
    else:
        splits = text.split(splitter)

    results = None
    if callable(parser):
        results = parser(splits)
    elif isinstance(parser, str):
        results = map(partial(re_parse, parser), splits)
    else:
        results = splits
    return list(filter(lambda x: x or x, map(mapper, results)))


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
