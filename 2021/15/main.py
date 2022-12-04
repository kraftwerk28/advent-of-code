import sys
from heapq import heappop, heappush


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
    queue, visited = [(0, s)], {s}
    # Dijkstra shortest path algorithm
    # P.S. I wonder if it's possible to use DP
    while queue:
        _, c = heappop(queue)
        i, j = c
        visited.add(c)
        neighbors = filter(
            lambda p: p not in visited and p in m,
            ((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)),
        )
        for n in neighbors:
            tentative = d[c] + m[n]
            if tentative < d[n]:
                d[n] = tentative
                heappush(queue, (tentative, n))
    return d[(maxi, maxj)]


if __name__ == "__main__":
    cave = {
        (i, j): int(c)
        for i, line in enumerate(sys.stdin.read().splitlines())
        for j, c in enumerate(line)
    }
    print(f"part 1: {part1(cave)}")
    print(f"part 2: {part1(repeat_map(cave))}")
