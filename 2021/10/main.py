import sys
from functools import reduce

pairs = [
    ("(", ")", 3, 1),
    ("[", "]", 57, 2),
    ("{", "}", 1197, 3),
    ("<", ">", 25137, 4),
]


def score(c):
    return next((s for x, y, s, _ in pairs if c in (x, y)))


def rev(c):
    return next((y if c == x else x for x, y, _, _ in pairs if c in (x, y)))


def matches(a, b):
    return rev(a) == b


def part1(lines):
    result = 0
    for line in lines:
        stack = []
        for c in line:
            if c in "([{<":
                stack.append(c)
            elif matches(c, stack[-1]):
                stack.pop()
            else:
                result += score(c)
                break
    return result


def part2(lines):
    def get_score(c):
        return next((s for x, _, _, s in pairs if x == c))
    scores = []
    for line in lines:
        stack = []
        corrupted = False
        for c in line:
            if c in "([{<":
                stack.append(c)
            elif matches(c, stack[-1]):
                stack.pop()
            else:
                corrupted = True
                break
        if not corrupted:
            stack.reverse()
            scores.append(reduce(lambda acc, c: acc*5+get_score(c), stack, 0))
    return sorted(scores)[len(scores)//2]


if __name__ == "__main__":
    lines = [line for line in sys.stdin.read().splitlines() if line]
    print(f"part 1: {part1(lines)}")
    print(f"part 2: {part2(lines)}")
