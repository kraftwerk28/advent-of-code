import re


def pt_range(p1, p2, diagonal=False):
    (x1, y1), (x2, y2) = p1, p2
    dx = x2 - x1
    dx = 0 if x1 == x2 else 1 if x1 < x2 else -1
    dy = 0 if y1 == y2 else 1 if y1 < y2 else -1
    if dx != 0 and dy != 0:
        if not diagonal or abs(x1-x2) != abs(y1-y2):
            return
    while x1 != x2 or y1 != y2:
        yield x1, y1
        x1 += dx
        y1 += dy
    yield x1, y1


def parse(fname):
    with open(fname) as f:
        lines = f.read().splitlines()
    res = []
    for line in lines:
        a, b, c, d = [
            int(n) for n in
            re.match(r"(\d+),(\d+) -> (\d+),(\d+)", line).groups()
        ]
        res.append(((a, b), (c, d)))
    return res


def part1(input):
    field = {}
    for p1, p2 in input:
        for pt in pt_range(p1, p2):
            field[pt] = field.get(pt, 0)+1
    return len([v for v in field.values() if v >= 2])


def part2(input):
    field = {}
    for p1, p2 in input:
        for pt in pt_range(p1, p2, diagonal=True):
            field[pt] = field.get(pt, 0)+1
    return len([v for v in field.values() if v >= 2])


if __name__ == "__main__":
    parsed = parse("input/05.txt")
    print(f"part 1: {part1(parsed)}")
    print(f"part 1: {part2(parsed)}")
