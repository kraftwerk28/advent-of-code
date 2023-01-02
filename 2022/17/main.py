import sys
import time


def parse_template(raw):
    result = []
    lines = [line for line in raw.splitlines() if line.strip()]
    lines.reverse()
    for ri, row in enumerate(lines):
        if not row:
            continue
        for ci, c in enumerate(row.strip()):
            if c == "#":
                result.append((ri, ci))
    return result


class Figure:
    def __init__(self, template, position):
        self.template = template
        self.width = max(c for _, c in template) + 1
        self.height = max(r for r, _ in template) + 1
        self.position = position

    def exec_command(self, command, scene):
        pr, pc = self.position
        if command == "<":
            pos_2 = (pr, pc - 1)
        elif command == ">":
            pos_2 = (pr, pc + 1)
        return self.try_move(pos_2, scene)

    def fall(self, scene):
        pr, pc = self.position
        pos_2 = (pr - 1, pc)
        return self.try_move(pos_2, scene)

    def try_move(self, position, scene):
        pr, pc = position
        blocks_2 = [(tr + pr, tc + pc) for tr, tc in self.template]
        if any(b in scene for b in blocks_2):
            return False
        if any(b[0] < 0 or b[1] < 0 or b[1] >= 7 for b in blocks_2):
            return False
        self.position = position
        return True

    def bake(self, scene):
        pr, pc = self.position
        for tr, tc in self.template:
            scene[(tr + pr, tc + pc)] = True


shapes_raw = [
    """
    ####
    """,
    """
    .#.
    ###
    .#.
    """,
    """
    ..#
    ..#
    ###
    """,
    """
    #
    #
    #
    #
    """,
    """
    ##
    ##
    """
]

shape_defs = [parse_template(s) for s in shapes_raw]
commands = sys.stdin.readline().strip()


def print_scene(scene, tower_height):
    for ri in range(tower_height + 3, -1, -1):
        linegen = (
            "#" if (ri, ci) in scene else "."
            for ci in range(0, 7)
        )
        print("".join(linegen))
    print()


def part_1():
    scene = {}
    tower_height = 0

    cmd_index = 0
    shape_index = 0
    nfigures = 0

    while nfigures < 2022:
        spawn_position = (tower_height + 3, 2)
        fig = Figure(shape_defs[shape_index], spawn_position)
        shape_index = (shape_index + 1) % len(shape_defs)
        while True:
            command = commands[cmd_index]
            fig.exec_command(command, scene)
            cmd_index = (cmd_index + 1) % len(commands)
            if not fig.fall(scene):
                fig.bake(scene)
                nfigures += 1
                new_height = fig.position[0] + fig.height
                if new_height > tower_height:
                    tower_height = new_height
                break
    print(f"Part 1: {tower_height}")


def part_2():
    scene = {}
    tower_height = 0

    cmd_index = 0
    shape_index = 0
    nfigures = 0

    bignum = 1000000000000
    prev_nfigures = 0
    prev_tower_height = 0

    while True:
        spawn_position = (tower_height + 3, 2)
        fig = Figure(shape_defs[shape_index], spawn_position)
        shape_index = (shape_index + 1) % len(shape_defs)
        while True:
            command = commands[cmd_index]
            if cmd_index == 0:
                print(f"{nfigures = } {tower_height = } {nfigures - prev_nfigures} {tower_height - prev_tower_height}")
                prev_nfigures = nfigures
                prev_tower_height = tower_height
                time.sleep(0.1)
            fig.exec_command(command, scene)
            cmd_index = (cmd_index + 1) % len(commands)
            if not fig.fall(scene):
                fig.bake(scene)
                nfigures += 1
                new_height = fig.position[0] + fig.height
                if new_height > tower_height:
                    tower_height = new_height
                break

    # tower_height = tower_height * (bignum // nfigures)
    # remaining_figures = bignum % nfigures
    # nfigures = 0
    # while nfigures < remaining_figures:
    #     spawn_position = (tower_height + 3, 2)
    #     fig = Figure(shape_defs[shape_index], spawn_position)
    #     shape_index = (shape_index + 1) % len(shape_defs)
    #     while True:
    #         command = commands[cmd_index]
    #         fig.exec_command(command, scene)
    #         cmd_index = (cmd_index + 1) % len(commands)
    #         if not fig.fall(scene):
    #             fig.bake(scene)
    #             nfigures += 1
    #             new_height = fig.position[0] + fig.height
    #             if new_height > tower_height:
    #                 tower_height = new_height
    #             break

    print(f"Part 2: {tower_height = }")


part_1()
part_2()
