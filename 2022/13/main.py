import sys
from functools import cmp_to_key

pairs = []
p1, p2 = None, None
for line in sys.stdin:
    if p1 is None:
        p1 = eval(line)
    elif p2 is None:
        p2 = eval(line)
    else:
        if type(p1) is int:
            p1 = [p1]
        if type(p2) is int:
            p2 = [p2]
        pairs.append((p1, p2))
        p1, p2 = None, None
if p1 and p2:
    pairs.append((p1, p2))


def compare(a, b):
    if type(a) is int and type(b) is int:
        return b - a
    if type(a) is int:
        return compare([a], b)
    if type(b) is int:
        return compare(a, [b])
    for ea, eb in zip(a, b):
        cmp = compare(ea, eb)
        if cmp != 0:
            return cmp
    return len(b) - len(a)


p1 = 0
for i, (a, b) in enumerate(pairs):
    if compare(a, b) > 0:
        p1 += i + 1
print(f"Part 1: {p1}")

packets = []
for a, b in pairs:
    packets.append(a)
    packets.append(b)
packets.append([[2]])
packets.append([[6]])
packets.sort(key=cmp_to_key(compare), reverse=True)
p2 = (packets.index([[2]]) + 1) * (packets.index([[6]]) + 1)
print(f"Part 2: {p2}")
