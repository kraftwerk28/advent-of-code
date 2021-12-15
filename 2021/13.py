import sys


def part2(points, folds):
    pts = points.copy()
    for axis, n in folds:
        if axis == "x":
            for p in [p for p in pts if p[0] > n]:
                pts.remove(p)
                pts.add((2*n-p[0], p[1]))
        else:
            for p in [p for p in pts if p[1] > n]:
                pts.remove(p)
                pts.add((p[0], 2*n-p[1]))
    for x, y in pts:
        sys.stdout.write(f"\033[{y+2};{x+2}H#")
    sys.stdout.write("\033[50;50H.")


def part1(points, folds):
    pts = points.copy()
    axis, n = folds[0]
    if axis == "x":
        for p in [p for p in pts if p[0] > n]:
            pts.remove(p)
            pts.add((2*n-p[0], p[1]))
    else:
        for p in [p for p in pts if p[1] > n]:
            pts.remove(p)
            pts.add((p[0], 2*n-p[1]))
    return len(pts)


if __name__ == "__main__":
    with open("input/13.txt") as f:
        points, folds = set(), []
        pts, instr = f.read().split("\n\n", maxsplit=2)
        for p in pts.splitlines():
            x, y = p.split(",")
            points.add((int(x), int(y)))
        for p in instr.splitlines():
            *_, w = p.split()
            axis, n = w.split("=")
            folds.append((axis, int(n)))
    print(f"part 1: {part1(points, folds)}")
    part2(points, folds)
    # print(f"part 2: {part2(points, folds)}")
