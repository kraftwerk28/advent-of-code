import sys


def step(m):
    unstable = []
    for k in m:
        m[k] += 1
        if m[k] == 10:
            unstable.append(k)
    while unstable:
        new_unstable = []
        for i, j in unstable:
            sib = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1),
                   (i - 1, j - 1), (i - 1, j + 1),
                   (i + 1, j - 1), (i + 1, j + 1)]
            for sibp in sib:
                if sibp not in m:
                    continue
                m[sibp] += 1
                if m[sibp] == 10:
                    new_unstable.append(sibp)
        unstable = new_unstable
    flashes = 0
    for k, v in m.items():
        if v > 9:
            m[k] = 0
            flashes += 1
    return flashes


def part1(m):
    result = 0
    for _ in range(100):
        result += step(m)
    return result


def part2(m):
    result = 0
    while True:
        step(m)
        result += 1
        if all(v == 0 for v in m.values()):
            return result


def dump(m):
    print("\n".join("".join(str(m[(i, j)])
          for j in range(5)) for i in range(5)))


if __name__ == "__main__":
    m = {}
    for i, line in enumerate(sys.stdin):
        for j, c in enumerate(line.strip()):
            m[(i, j)] = int(c)
    print(f"part 1: {part1(m.copy())}")
    print(f"part 2: {part2(m.copy())}")
