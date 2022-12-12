import sys


def part_1(lines: list[str]):
    cycle = 0
    x = 1
    stren = 0

    def add_cycle():
        nonlocal cycle, stren
        cycle += 1
        if (cycle - 20) % 40 == 0:
            stren += cycle * x

    for line in lines:
        match line.split():
            case ["noop"]:
                add_cycle()
            case ["addx", am]:
                add_cycle()
                add_cycle()
                x += int(am)

    print(f"Part 1: {stren}")


def part_2(lines: list[str]):
    cycle = 0
    x = 1
    pixels = [[" "] * 40 for _ in range(6)]

    def draw():
        row = pixels[cycle // 40]
        col = cycle % 40
        if col >= x - 1 and col <= x + 1:
            row[col] = "#"

    for line in lines:
        match line.split():
            case ["noop"]:
                draw()
                cycle += 1
            case ["addx", am]:
                draw()
                cycle += 1
                draw()
                cycle += 1
                x += int(am)

    print("Part 2:")
    print("\n".join("".join(line) for line in pixels))


lines = sys.stdin.read().splitlines()
part_1(lines)
part_2(lines)
