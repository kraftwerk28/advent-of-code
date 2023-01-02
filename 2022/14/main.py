import sys

ROCK = 1
SAND = 2
SPAWN = 3

pt_map = {ROCK: "█", SAND: "░", SPAWN: "+"}


def parse_pos(s):
    x, y = s.split(",")
    return int(x), int(y)


def print_scene(scene, padding=1):
    min_row, max_row = min(a for _, a in scene), max(a for _, a in scene)
    min_col, max_col = min(a for a, _ in scene), max(a for a, _ in scene)
    for rowi in range(min_row - padding, max_row + padding + 1):
        linegen = (
            pt_map.get(scene.get((coli, rowi), 0), ".")
            for coli in range(min_col - padding, max_col + padding + 1)
        )
        print("".join(linegen))
    print()


def arange(start, end):
    d = 1 if start < end else -1
    while start != end:
        yield start
        start += d
    yield start


scene = {}
for line in sys.stdin:
    corners = [parse_pos(s) for s in line.split(" -> ")]
    cstart = corners[0]
    for cend in corners[1:]:
        if cstart[0] == cend[0]:
            for i in arange(cstart[1], cend[1]):
                scene[(cstart[0], i)] = ROCK
        if cstart[1] == cend[1]:
            for i in arange(cstart[0], cend[0]):
                scene[(i, cstart[1])] = ROCK
        cstart = cend


# print_scene(scene, padding=5)


def part_1():
    scene_copy = scene.copy()
    max_y = max(y for _, y in scene_copy.keys())
    trapped_sand = 0
    while True:
        particle = (500, 0)
        while True:
            below = (particle[0], particle[1] + 1)
            left_below = (particle[0] - 1, particle[1] + 1)
            right_below = (particle[0] + 1, particle[1] + 1)
            if particle[1] > max_y:
                print(f"Part 1: {trapped_sand}")
                return
            elif below not in scene_copy:
                particle = below
            elif left_below not in scene_copy:
                particle = left_below
            elif right_below not in scene_copy:
                particle = right_below
            else:
                scene_copy[particle] = SAND
                trapped_sand += 1
                # print_scene(scene_copy, padding=3)
                break


def part_2():
    scene_copy = scene.copy()
    floor_y = max(y for _, y in scene_copy.keys()) + 2
    trapped_sand = 0
    while True:
        particle = (500, 0)
        if particle in scene_copy:
            print(f"Part 2: {trapped_sand}")
            print_scene(scene_copy, padding=4)
            return
        while True:
            below = (particle[0], particle[1] + 1)
            left_below = (particle[0] - 1, particle[1] + 1)
            right_below = (particle[0] + 1, particle[1] + 1)
            if particle[1] == floor_y - 1:
                scene_copy[particle] = SAND
                trapped_sand += 1
                break
            elif below not in scene_copy:
                particle = below
            elif left_below not in scene_copy:
                particle = left_below
            elif right_below not in scene_copy:
                particle = right_below
            else:
                scene_copy[particle] = SAND
                trapped_sand += 1
                # print_scene(scene_copy, padding=3)
                break


part_1()
part_2()
