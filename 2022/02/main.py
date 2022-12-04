#!/usr/bin/env python
import sys

win = ["A Y", "B Z", "C X"]
draw = ["A X", "B Y", "C Z"]
lose = ["A Z", "B X", "C Y"]
itemscore = {"X": 1, "Y": 2, "Z": 3}
part2_strat = {"X": lose, "Y": draw, "Z": win}


def score(line):
    ret = itemscore[line[-1]]
    if line in win:
        return ret + 6
    if line in draw:
        return ret + 3
    return ret


def part2line(line):
    comb = part2_strat[line[-1]]
    return next(c for c in comb if c[0] == line[0])


lines = [line for line in sys.stdin.read().splitlines() if line]

print(f"Part 1: {sum(score(line) for line in lines)}")
print(f"Part 2: {sum(score(part2line(line)) for line in lines)}")
