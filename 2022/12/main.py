import sys

lines = sys.stdin.read().splitlines()
hmap = {
    (ri, ci): ord(c) for ri, line in enumerate(lines)
    for ci, c in enumerate(line)
}
start = next(p for p, c in hmap.items() if c == ord("S"))
end = next(p for p, c in hmap.items() if c == ord("E"))
hmap[start] = ord("a")
hmap[end] = ord("z")

unvisited = {p: 0xffff for p in hmap}
unvisited[start] = 0

while unvisited:
    minp = min(unvisited, key=lambda k: unvisited[k])
    if minp == end:
        print(f"Part 1: {unvisited[minp]}")
        break
    min_cost = unvisited[minp]
    del unvisited[minp]
    ri, ci = minp
    neigh = [(ri + 1, ci), (ri, ci + 1), (ri - 1, ci), (ri, ci - 1)]
    for n in neigh:
        if n not in unvisited:
            continue
        if hmap[n] > hmap[minp] + 1:
            continue
        alt = min_cost + 1
        if alt < unvisited[n]:
            unvisited[n] = alt

unvisited = {p: 0xffff for p in hmap}
unvisited[end] = 0
while unvisited:
    minp = min(unvisited, key=lambda k: unvisited[k])
    if hmap[minp] == ord("a"):
        print(f"Part 2: {unvisited[minp]}")
        break
    min_cost = unvisited[minp]
    del unvisited[minp]
    ri, ci = minp
    neigh = [(ri + 1, ci), (ri, ci + 1), (ri - 1, ci), (ri, ci - 1)]
    for n in neigh:
        if n not in unvisited:
            continue
        if hmap[minp] > hmap[n] + 1:
            continue
        alt = min_cost + 1
        if alt < unvisited[n]:
            unvisited[n] = alt
