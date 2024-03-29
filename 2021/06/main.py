import sys
from functools import cache


@cache
def nfish_after_n_days(n):
    if n <= 7:
        return 1
    res = nfish_after_n_days(n - 7) + nfish_after_n_days(n - 9)
    return res


def simulate(input, days):
    numbers = [int(n) for n in input[0].split(",")]
    return sum(nfish_after_n_days(days + (7 - n)) for n in numbers)


if __name__ == "__main__":
    lines = sys.stdin.read().splitlines()
    print(f"Part 1: {simulate(lines, 80)}")
    print(f"Part 2: {simulate(lines, 256)}")
