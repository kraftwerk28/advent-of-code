import sys

lines = [line.strip() for line in sys.stdin.read().splitlines()]
lines = [line for line in lines if line]
width, height = len(lines[0]), len(lines)

EAST = True
SOUTH = False


def print_trench(trench):
    for rowi in range(height):
        linegen = (
            "." if (rowi, coli) not in trench
            else ">" if trench[(rowi, coli)] == EAST
            else "v"
            for coli in range(width)
        )
        print("".join(linegen))


def step(trench):
    result = {}
    moved = False
    for pos, dir in trench.items():
        if dir == SOUTH:
            continue
        rowi, coli = pos
        forward = (rowi, (coli + 1) % width)
        if forward in trench:
            result[pos] = dir
        else:
            result[forward] = dir
            moved = True
    for pos, dir in trench.items():
        if dir == EAST:
            continue
        rowi, coli = pos
        forward = ((rowi + 1) % height, coli)
        if result.get(forward, None) == EAST or trench.get(forward, None) == SOUTH:
            result[pos] = dir
        else:
            result[forward] = dir
            moved = True
    # return result
    return result, moved


def part_1(trench):
    steps = 0
    while True:
        steps += 1
        trench, moved = step(trench)
        if not moved:
            break
    print(f"Part 1: {steps}")


def part_2(trench):
    pass


trench = {
    (rowi, coli): EAST if c == ">" else SOUTH
    for rowi, row in enumerate(lines)
    for coli, c in enumerate(row)
    if c != "."
}

part_1(trench)
part_2(trench)
