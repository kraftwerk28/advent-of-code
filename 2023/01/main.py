import sys

digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

lines = sys.stdin.read().splitlines()


def line_to_value(line: str):
    if not line:
        return 0
    d1 = next((c for c in line if c.isdigit()), 0)
    d2 = next((c for c in reversed(line) if c.isdigit()), 0)
    return int(f"{d1}{d2}")


def line_to_value_2(line: str):
    if not line:
        return 0
    d1_pos, d2_pos = len(line), -1
    for ith, digit in enumerate(digits):
        str_pos = line.find(digit)
        if str_pos >= 0 and str_pos < d1_pos:
            d1_pos, d1 = str_pos, ith + 1
        str_pos = line.rfind(digit)
        if str_pos >= 0 and str_pos > d2_pos:
            d2_pos, d2 = str_pos, ith + 1
    str_pos, digit = next(
        ((i, c) for i, c in enumerate(line) if c.isdigit()),
        (len(line), None),
    )
    if str_pos < d1_pos:
        d1 = digit
    str_pos, digit = next(
        ((i, c) for i, c in reversed(list(enumerate(line))) if c.isdigit()),
        (0, None),
    )
    if str_pos > d2_pos:
        d2 = digit
    return int(f"{d1}{d2}")


print(sum(line_to_value(line) for line in lines))
print(sum(line_to_value_2(line) for line in lines))
