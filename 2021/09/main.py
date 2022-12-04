import sys
from functools import reduce
from operator import mul


def adj(p):
    i, j = p
    return [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]


def find_basin_points(m, p):
    queue = [p]
    result = set([p])
    while queue:
        current = queue.pop(0)
        for adjacent in adj(current):
            if (adjacent in result) or m.get(adjacent, 9) == 9:
                continue
            queue.append(adjacent)
            result.add(adjacent)
    return result


def part1(m):
    lowpoints = [
        v
        for k, v in m.items()
        if all(v < m.get(x, 0xffff) for x in adj(k))
    ]
    return sum(p+1 for p in lowpoints)


# def draw_matrix(m):
#     height = max(r for r, c in m.keys())+1
#     width = max(c for r, c in m.keys())+1
#     for i in range(width):
#         # sys.stdout.write(f"\033[{1};{i+1}H#")
#         sys.stdout.write(f"\033[{height+3};{i+1}H#")
#     # for (row, col), v in m.items():
#     #     sys.stdout.write(f"\033[{row+2};{col+2}H{v}")


def part2(m):
    lowpoints = [
        k for k, v in m.items()
        if all(v < m.get(x, 0xffff) for x in adj(k))
    ]
    basins = [find_basin_points(m, p) for p in lowpoints]
    basin_sizes = [len(b) for b in basins]
    return reduce(mul, sorted(basin_sizes)[-3:])
    # return reduce(mul, (len(b) for b in sorted(basins, key=lambda l: len(l))[-3:]))


if __name__ == "__main__":
    m = {}
    for i, line in enumerate(sys.stdin):
        for j, c in enumerate(line.strip()):
            m[(i, j)] = int(c)
    print(f"part 1: {part1(m)}")
    print(f"part 2: {part2(m)}")
