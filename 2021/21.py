import re
from functools import cache, reduce
from dataclasses import dataclass


@dataclass
class Foo:
    user: int


def part1(p1, p2):
    rolled = -3
    nrolled = 0
    s1, s2 = 0, 0
    while True:
        rolled += 9
        nrolled += 3
        p1 = ((p1 - 1 + rolled) % 10) + 1
        s1 += p1
        if s1 >= 1000:
            break
        rolled += 9
        nrolled += 3
        p2 = ((p2 - 1 + rolled) % 10) + 1
        s2 += p2
        if s2 >= 1000:
            break
    return (s2 if s2 < s1 else s1) * nrolled


def tuple_sum(a, b):
    c, d = a
    e, f = b
    return (c + e, d + f)


def part2(*args):
    def next_state(g, outcome):
        first_player, p1, p2, score1, score2 = g
        if first_player:
            p1 = ((p1 - 1 + outcome) % 10) + 1
            score1 += p1
            return (False, p1, p2, score1, score2)
        else:
            p2 = ((p2 - 1 + outcome) % 10) + 1
            score2 += p2
            return (True, p1, p2, score1, score2)

    @cache
    def ans_for_cube_throw(g):
        """ 1st player wins / total games """
        _, _, _, score1, score2 = g
        if score1 >= 21:
            return 1, 1
        if score2 >= 21:
            return 0, 1
        a = next_state(g, 1)
        b = next_state(g, 2)
        c = next_state(g, 3)
        return reduce(tuple_sum, map(ans_for_cube_throw, (a, b, c)))

    return ans_for_cube_throw((False, 4, 8, 0, 0))


# Sample
# 444356092776315 - player 1
# 341960390180808 - player 2

if __name__ == "__main__":
    cls = Foo(12)
    cls2 = Foo(**cls.__dict__)
    print(cls2)
    exit(0)
    with open("input/21.txt") as f:
        m = re.findall(r"Player \d starting position: (\d+)", f.read())
        p1, p2 = map(int, m)
    print(f"part 1: {part1(4, 8)}")
    print(f"part 2: {part2(4, 8)}")
    # print(f"part 1: {part1(p1, p2)}")
    # print(f"part 2: {part2(p1, p2)}")
