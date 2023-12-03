import sys
import re

lines = [line for line in sys.stdin.read().splitlines() if line]

assert all(len(line) == len(lines[0]) for line in lines)

part_numbers = []

for row, line in enumerate(lines):
    for match in re.finditer(r"\d+", line):
        span_start, span_end = match.span()
        adjacents = []
        if row > 0:
            adjacents.extend(
                (row - 1, c)
                for c in range(span_start - 1, span_end + 1)
                if c >= 0 and c < len(line)
            )
        if span_start > 0:
            adjacents.append((row, span_start - 1))
        if span_end < len(line):
            adjacents.append((row, span_end))
        if row < len(lines) - 1:
            adjacents.extend(
                (row + 1, c)
                for c in range(span_start - 1, span_end + 1)
                if c >= 0 and c < len(line)
            )
        part_numbers.append((int(match[0]), adjacents))

part1 = 0
for num, adjacents in part_numbers:
    for r, c in adjacents:
        char = lines[r][c]
        if not char.isdigit() and char != ".":
            part1 += int(num)
            break

print(part1)

part2 = 0
gears = dict()
for num, adjacents in part_numbers:
    for pos in adjacents:
        row, col = pos
        if lines[row][col] == "*":
            gears.setdefault(pos, []).append(num)
print(sum(nums[0] * nums[1] for nums in gears.values() if len(nums) == 2))
