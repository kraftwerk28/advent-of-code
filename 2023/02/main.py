import sys

lines = sys.stdin.read().splitlines()


def parse_set(s: str):
    for cube_str in s.split(","):
        q, color = cube_str.split()
        yield int(q), color


def parse_line(line: str):
    title, content = line.split(":", 2)
    game_id = int(title[5:])
    sets = [{color: q for q, color in parse_set(s)} for s in content.split(";")]
    return game_id, sets


def is_in_limits(game, limits):
    for s in game[1]:
        for color, q in s.items():
            if q > limits.get(color, 0):
                return False
    return True


def minimal_set(game):
    qs = {}
    for s in game[1]:
        for color, q in s.items():
            qs.setdefault(color, []).append(q)
    return {color: max(q) for color, q in qs.items()}


def set_power(s):
    return s["red"] * s["green"] * s["blue"]


part1_limits = dict(red=12, green=13, blue=14)

games = [parse_line(line) for line in lines if line]

print(sum(game[0] for game in games if is_in_limits(game, part1_limits)))
print(sum(set_power(minimal_set(game)) for game in games))
