from collections import defaultdict


def part(polymer, reactions, steps):
    rules = {k: (k[0]+v, v+k[1]) for k, v in reactions.items()}
    pair_counts = defaultdict(int)
    for p in zip(polymer, polymer[1:]):
        pair_counts[p[0] + p[1]] += 1
    for _ in range(steps):
        new_pair_counts = defaultdict(int)
        for k, v in pair_counts.items():
            a, b = rules[k]
            new_pair_counts[a] += v
            new_pair_counts[b] += v
        pair_counts = new_pair_counts
    letter_counts = defaultdict(int)
    for p, c in pair_counts.items():
        letter_counts[p[0]] += c
    letter_counts[polymer[-1]] += 1
    return max(letter_counts.values()) - min(letter_counts.values())


if __name__ == "__main__":
    with open("input/14.txt") as f:
        polymer = f.readline().strip()
        f.readline()
        reactions = dict(line.strip().split(" -> ", maxsplit=2) for line in f)
    print(f"part 1: {part(polymer, reactions, 10)}")
    print(f"part 2: {part(polymer, reactions, 40)}")
