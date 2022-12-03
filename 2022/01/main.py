import sys

elfs = [[]]
for line in sys.stdin.read().splitlines():
    if line:
        elfs[-1].append(int(line))
    else:
        elfs.append([])
sums = [sum(elf) for elf in elfs]
print(f"Part 1: {max(sums)}")
print(f"Part 2: {sum(sorted(sums, reverse=True)[:3])}")
