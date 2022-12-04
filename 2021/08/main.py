import sys
from itertools import permutations

#  00000
# 1     2
# 1     2
#  33333
# 4     5
# 4     5
#  66666

segconfig = {
    0: (0, 1, 2, 4, 5, 6),
    1: (2, 5),
    2: (0, 2, 3, 4, 6),
    3: (0, 2, 3, 5, 6),
    4: (1, 2, 3, 5),
    5: (0, 1, 3, 5, 6),
    6: (0, 1, 3, 4, 5, 6),
    7: (0, 2, 5),
    8: (0, 1, 2, 3, 4, 5, 6),
    9: (0, 1, 2, 3, 5, 6),
}


# def is_valid_number(wiring, segments):
#     segset = set(segments)
#     return any(
#         segset == set(wiring[n] for n in si) for si in segconfig.values()
#     )


def wiring_to_number(wiring, num):
    for n, si in segconfig.items():
        if set(num) == set(wiring[i] for i in si):
            return n
    # return next((num for num, si in segconfig.items()
    #              if set(num) == set(wiring[i] for i in si)),
    #             None)


def part1(input):
    return sum(
        len([x for x in out if len(x) in (2, 3, 4, 7)]) for _, out in input
    )


def part2(input):
    result = 0
    for renders, output in input:
        for wiring in permutations("abcdefg"):
            if all(wiring_to_number(wiring, num) is not None
                   for num in renders):
                output_num = "".join(
                    str(wiring_to_number(wiring, num)) for num in output
                )
                result += int(output_num)
    return result


if __name__ == "__main__":
    input = []
    for line in sys.stdin:
        a, b = line.split(" | ")
        input.append((a.split(), b.split()))
    print(f"part 1: {part1(input)}")
    print(f"part 2: {part2(input)}")
