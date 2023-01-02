import sys


cubes = set()
for line in sys.stdin:
    x, y, z = [int(n) for n in line.split(",")]
    cubes.add((x, y, z))
bound_1 = (
    min(x for x, _, _ in cubes) - 1,
    min(y for _, y, _ in cubes) - 1,
    min(z for _, _, z in cubes) - 1,
)
bound_2 = (
    max(x for x, _, _ in cubes) + 1,
    max(y for _, y, _ in cubes) + 1,
    max(z for _, _, z in cubes) + 1,
)


def siblings(voxel):
    x, y, z = voxel
    sides = [
        (x - 1, y, z),
        (x + 1, y, z),
        (x, y - 1, z),
        (x, y + 1, z),
        (x, y, z - 1),
        (x, y, z + 1),
    ]
    x1, y1, z1 = bound_1
    x2, y2, z2 = bound_2
    return [
        side
        for side in sides
        if side[0] >= x1
        and side[0] <= x2
        and side[1] >= y1
        and side[1] <= y2
        and side[2] >= z1
        and side[2] <= z2
    ]


p1 = 0
for cube in cubes:
    p1 += 6
    for sib in siblings(cube):
        if sib in cubes:
            p1 -= 1
print(f"Part 1: {p1}")

p2 = 0
queue = set([bound_1])
visited = set()
while queue:
    air_cube = queue.pop()
    visited.add(air_cube)
    for sib in siblings(air_cube):
        if sib in queue or sib in visited:
            continue
        if sib in cubes:
            p2 += 1
        else:
            queue.add(sib)
print(f"Part 2: {p2}")
