import re
from functools import cache
import itertools
import sys


def part1(p1, p2):
    diestate = 1  # the number die will output on the next roll
    dierolls = 0  # times the die has been rolled
    p1score, p2score = 0, 0  # total scores
    p1turn = True  # whose player turn is next
    while True:
        if p1score >= 1000:
            return p2score * dierolls
        elif p2score >= 1000:
            return p1score * dierolls
        else:
            dierolls += 3
            jumplen = 3*diestate + 3
            diestate = (diestate-1+3) % 100 + 1
            if p1turn:
                p1 = (p1 - 1 + jumplen) % 10 + 1
                p1score += p1
            else:
                p2 = (p2 - 1 + jumplen) % 10 + 1
                p2score += p2
            p1turn = not p1turn


@cache
def part2(p1pos, p2pos, p1turn=True, p1score=0, p2score=0):
    if p1score >= 21:
        return 1, 0
    if p2score >= 21:
        return 0, 1
    p1wins, p2wins = 0, 0
    for roll in itertools.product([1, 2, 3], repeat=3):
        jumplen = sum(roll)
        if p1turn:
            newpos = (p1pos - 1 + jumplen) % 10 + 1
            newscore = p1score + newpos
            p1w, p2w = part2(newpos, p2pos, not p1turn, newscore, p2score)
        else:
            newpos = (p2pos - 1 + jumplen) % 10 + 1
            newscore = p2score + newpos
            p1w, p2w = part2(p1pos, newpos, not p1turn, p1score, newscore)
        p1wins += p1w
        p2wins += p2w
    return p1wins, p2wins


# Sample
# 444356092776315 - player 1
# 341960390180808 - player 2
if __name__ == "__main__":
    m = re.findall(r"Player \d starting position: (\d+)", sys.stdin.read())
    p1, p2 = map(int, m)
    p1ans = part1(p1, p2)
    print(f"part 1: {p1ans}")
    p2ans = max(part2(p1, p2))
    print(f"part 2: {p2ans}")
