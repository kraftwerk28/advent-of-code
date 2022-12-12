import sys


def parse_line(line):
    dir, am = line.split()
    return dir, int(am)


def push_point(p, dir):
    row, col = p
    match dir:
        case "U":
            return (row - 1, col)
        case "R":
            return (row, col + 1)
        case "D":
            return (row + 1, col)
        case "L":
            return (row, col - 1)


def tail_pos(head, tail):
    dr, dc = head[0] - tail[0], head[1] - tail[1]
    if abs(dr) <= 1 and abs(dc) <= 1:
        dr, dc = 0, 0
    else:
        if abs(dr) >= 2:
            dr = 1 if dr > 0 else -1
        if abs(dc) >= 2:
            dc = 1 if dc > 0 else -1
    return (tail[0] + dr, tail[1] + dc)


def part_1(commands):
    h = (0, 0)
    t = (0, 0)
    visited = set()
    for dir, amount in commands:
        for _ in range(amount):
            h = push_point(h, dir)
            t = tail_pos(h, t)
            visited.add(t)
    print(f"Part 1: {len(visited)}")


def part_2(commands):
    rope = [(0, 0)] * 10
    visited = set()
    for dir, amount in commands:
        for _ in range(amount):
            rope_2 = [push_point(rope[0], dir)]
            for k in rope[1:]:
                rope_2.append(tail_pos(rope_2[-1], k))
            rope = rope_2
            visited.add(rope[-1])
    # print_points(visited, padding=2)
    print(f"Part 2: {len(visited)}")


def print_rope(rope):
    min_row, max_row = min(a for a, _ in rope), max(a for a, _ in rope)
    min_col, max_col = min(a for _, a in rope), max(a for _, a in rope)
    for rowi in range(min_row - 1, max_row + 2):
        line = ""
        for coli in range(min_col - 1, max_col + 2):
            try:
                rope_char = rope.index((rowi, coli))
            except ValueError:
                rope_char = "."
            if rope_char == 0:
                rope_char = "H"
            line += str(rope_char)
        print("".join(line))
    print()


def print_points(points, padding=1):
    min_row, max_row = min(a for a, _ in points), max(a for a, _ in points)
    min_col, max_col = min(a for _, a in points), max(a for _, a in points)
    for rowi in range(min_row - padding, max_row + padding + 1):
        linegen = (
            "#" if (rowi, coli) in points
            else "."
            for coli in range(min_col - padding, max_col + padding + 1)
        )
        print("".join(linegen))
    print()


commands = [parse_line(line) for line in sys.stdin.read().splitlines()]
part_1(commands)
part_2(commands)
