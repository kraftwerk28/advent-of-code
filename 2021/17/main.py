from itertools import takewhile, product
import re
import sys


def sim(velocity):
    x, y = 0, 0
    vx, vy = velocity
    while True:
        x, y = x + vx, y + vy
        yield x, y, vx, vy
        vx += 1 if vx < 0 else -1 if vx > 0 else 0
        vy -= 1


def part1(minx, maxx, miny, maxy):
    start_vy = 0
    successfull = []
    for start_vy in range(200):
        simulation = sim((0, start_vy))
        points = list(takewhile(lambda p: p[1] >= miny, simulation))
        if points[-1][1] <= maxy:
            successfull.append((start_vy, points))
    return max(p[1] for p in successfull[-1][1])


def part2(minx, maxx, miny, maxy):
    result = 0
    # This is kinda slow ngl
    for start_vx, start_vy in product(range(-500, 501), repeat=2):
        simulation = sim((start_vx, start_vy))
        points = list(takewhile(
            lambda p: p[0] <= maxx and p[1] >= miny,
            simulation,
        ))
        if points and points[-1][0] >= minx and points[-1][1] <= maxy:
            result += 1
    return result


if __name__ == "__main__":
    pat = r"x=([-\d]+)\.{2}([-\d]+), y=([-\d]+)\.{2}([-\d]+)"
    m = re.search(pat, sys.stdin.read())
    minx, maxx, miny, maxy = map(int,  m.groups())
    print(f"part 1: {part1(minx, maxx, miny, maxy)}")
    print(f"part 2: {part2(minx, maxx, miny, maxy)}")
