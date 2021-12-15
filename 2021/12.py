def is_small(cave):
    return cave.islower() and cave != "start" and cave != "end"


def part1(nodes):
    def ways_to_cave(cave, visited_small=None):
        if visited_small is None:
            visited_small = [cave]
        if cave == "start":
            return 1
        incoming = []
        new_visited_small = visited_small[:]
        for edge in nodes:
            a, b = edge
            if cave == a:
                adj = b
            elif cave == b:
                adj = a
            else:
                continue
            if adj in visited_small:
                continue
            incoming.append(adj)
        if is_small(cave):
            new_visited_small.append(cave)
        return sum(
            ways_to_cave(c, visited_small=new_visited_small)
            for c in incoming
        )
    return ways_to_cave("end")


def part2(nodes):
    paths = set()

    def ways_to_cave(cave, visited_small=None, is_second=False, path=tuple()):
        if visited_small is None:
            visited_small = [cave]
        if cave == "start":
            paths.add(path)
            return 1
        incoming = []
        for edge in nodes:
            a, b = edge
            if cave == a:
                adj = b
            elif cave == b:
                adj = a
            else:
                continue
            if adj in visited_small:
                continue
            incoming.append(adj)
        if is_small(cave):
            s = sum(
                ways_to_cave(
                    c,
                    visited_small[:] + [cave],
                    is_second,
                    (*path, c),
                )
                for c in incoming
            )
            if not is_second:
                s += sum(
                    ways_to_cave(
                        c,
                        visited_small[:],
                        True,
                        (*path, c),
                    )
                    for c in incoming
                )
            return s
        else:
            return sum(
                ways_to_cave(c, visited_small[:], is_second, (*path, c))
                for c in incoming
            )
    ways_to_cave("end")
    return len(paths)


if __name__ == "__main__":
    with open("input/12.txt") as f:
        paths = []
        for line in f:
            f, t = line.strip().split("-", maxsplit=2)
            paths.append((f, t))
    print(f"part 1: {part1(paths)}")
    print(f"part 2: {part2(paths)}")
