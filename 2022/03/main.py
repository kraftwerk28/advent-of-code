#!/usr/bin/env python
import sys

lines = [line for line in sys.stdin.read().splitlines() if line]


def item_value(item):
    o = ord(item)
    return o - 96 if o >= 97 else o - 38


p1 = 0
for line in lines:
    comp1, comp2 = line[:len(line)//2], line[len(line)//2:]
    common = (set(comp1) & set(comp2)).pop()
    p1 += item_value(common)

p2 = 0
groups = [lines[i:i+3] for i in range(0, len(lines), 3)]
for group in groups:
    a, b, c = group
    common = (set(a) & set(b) & set(c)).pop()
    p2 += item_value(common)

print(f"Part 1: {p1}")
print(f"Part 2: {p2}")
