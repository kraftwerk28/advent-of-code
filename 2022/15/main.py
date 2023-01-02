import sys


def parse_line(line: str):
    parts = line.split()
    s1 = int(parts[2][2:-1])
    s2 = int(parts[3][2:-1])
    b1 = int(parts[-2][2:-1])
    b2 = int(parts[-1][2:])
    return (s1, s2), (b1, b2)


def merge_ranges(ranges):
    ranges_2 = sorted(ranges, key=lambda r: r[0])
    merged = ranges_2[:1]
    for r in ranges_2[1:]:
        rstart, rend = r
        last_rstart, last_rend = merged[-1]
        if last_rend < rstart:
            merged.append(r)
        elif rend <= last_rend:
            continue
        else:
            merged[-1] = (last_rstart, rend)
    return merged


sensors = [parse_line(line) for line in sys.stdin]


def part_1(sensors, target_y=10):
    ranges = []
    for (sx, sy), (bx, by) in sensors:
        if bx < sx:
            rx = bx - abs(by - sy)
        else:
            rx = 2*sx - bx - abs(by - sy)
        r = sx - rx
        if target_y > sy + r or target_y < sy - r:
            continue
        range_w = (2*r + 1) - abs(target_y - sy)*2
        range_x = rx + abs(target_y - sy)
        ranges.append((range_x, range_x + range_w))
    merged = merge_ranges(ranges)
    covered = 0
    for rstart, rend in merged:
        covered += rend - rstart

    all_objects = set(s for s, _ in sensors) | set(b for _, b in sensors)
    for (ox, oy) in all_objects:
        if oy == target_y and any(ox >= rs and ox <= re for rs, re in merged):
            covered -= 1

    print(f"Part 1: {covered}")


def part_2(sensors, min_bound=0, max_bound=4_000_000):
    slices = [[] for _ in range(max_bound + 1)]

    for (sx, sy), (bx, by) in sensors:
        if bx < sx:
            rx = bx - abs(by - sy)
        else:
            rx = 2*sx - bx - abs(by - sy)
        r = sx - rx

        # Top part
        for nslice in range(r):
            slice_y = sy - r + nslice
            if slice_y < min_bound or slice_y > max_bound:
                continue
            slice_x = rx + r - nslice
            slice_w = nslice * 2 + 1
            slices[slice_y].append((slice_x, slice_x + slice_w))

        # Mid part
        slices[sy].append((sx - r, sx + r))

        # Bottom part
        for nslice in range(r):
            slice_y = sy + nslice + 1
            if slice_y < min_bound or slice_y > max_bound:
                continue
            slice_x = rx + nslice + 1
            slice_w = (r - nslice) * 2 - 1
            slices[slice_y].append((slice_x, slice_x + slice_w))

    slices = [merge_ranges(ranges) for ranges in slices]

    for beacon_y, ranges in enumerate(slices):
        if len(ranges) > 1:
            beacon_x = ranges[0][1]
            result = beacon_x * 4000000 + beacon_y
            print(f"Part 2: {result}")
            break


part_1(sensors, target_y=2000000)
part_2(sensors, min_bound=0, max_bound=4000000)

# For the sample.txt
# part_1(sensors, target_y=10)
# part_2(sensors, min_bound=0, max_bound=20)

"""
sx = 12
sy = 7

bx = 9
by = 8

rx = 9 - 1 = 8
r = 12 - 8 = 4
..............................
..............................
..............................
............#.................
...........#.#................
..........#...#...............
.........#.....#..............
........#...#...#.............
.........S.....#..............
..........#...#...............
...........#.#................
............#.................
..............................
..............................
..............................
..............................
..............................
..............................
..............................
..............................
..............................
"""
