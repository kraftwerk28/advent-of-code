def repeat_map(m):
    maxi = len(set(i for i, _ in m.keys()))
    maxj = len(set(j for _, j in m.keys()))
    result = {}
    for i in range(maxi*5):
        for j in range(maxj*5):
            diff = i//maxi + j//maxj
            # print(i, j, diff)
            n = m[(i % maxi, j % maxj)] + diff
            if n > 9:
                n -= 9
            result[(i, j)] = n
    return result


def part1(m):
    maxi = max(i for i, _ in m.keys())
    maxj = max(j for _, j in m.keys())
    d = {p: 0xffff for p in m}
    s = (0, 0)
    d[s] = 0
    for _ in range(4):
        for c in m.keys():
            i, j = c
            nei = [
                p for p in
                ((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1))
                if p in m
            ]
            for n in nei:
                tentative = d[c] + m[n]
                if tentative < d[n]:
                    d[n] = tentative
    return d[(maxi, maxj)]


if __name__ == "__main__":
    with open("input/15.txt") as f:
        cave = {
            (i, j): int(c)
            for i, line in enumerate(f.read().splitlines())
            for j, c in enumerate(line)
        }
    print(f"part 1: {part1(cave)}")
    print(f"part 2: {part1(repeat_map(cave))}")
