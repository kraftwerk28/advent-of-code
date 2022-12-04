import sys
from dataclasses import dataclass
from heapq import heappush, heappop


MAP = [
    "#############",
    "#...........#",
    "###.#.#.#.###",
    "  #.#.#.#.#  ",
    "  #########  "
]

WALLS = {
    (rn, cn)
    for rn, line in enumerate(MAP)
    for cn, c in enumerate(line)
    if c == "#"
}

COST = {"A": 1, "B": 10, "C": 100, "D": 1000}
DEST_ROOM = {"A": 3, "B": 5, "C": 7, "D": 9}


@dataclass(frozen=True, eq=True)
class State:
    amphipods: dict[tuple[int, int], str]

    def __hash__(self):
        t = tuple(self.amphipods.items())
        return hash(t)

    def __lt__(self, o):
        return True

    @classmethod
    def from_str(cls, s):
        lines = [line for line in s.split("\n") if line.strip()]
        amph = {
            (rn, cn): c
            for rn, line in enumerate(lines)
            for cn, c in enumerate(line)
            if c in "ABCD"
        }
        return cls(amph)

    def possible_moves(self) -> list[tuple["State", int]]:
        ret = []
        for pos, amphipod in self.amphipods.items():
            row, col = pos
            # dirs = []
            # if row == 1:
            #     if col > 1:
            #         dirs.append((row, col - 1))
            #     if col < 11:
            #         dirs.append((row, col + 1))
            # if col == DEST_ROOM[amphipod]:
            # if col == DEST_ROOM[amphipod]:
            #     pass
            dirs = [(row - 1, col), (row, col - 1), (row, col + 1)]
            if row == 1 and col == DEST_ROOM[amphipod]:
                dirs.append((row + 1, col))
            rest = {p: a for p, a in self.amphipods.items() if p != pos}
            for nwse in dirs:
                if nwse not in WALLS and nwse not in self.amphipods.keys():
                    d = rest.copy()
                    d[nwse] = amphipod
                    s = State(d)
                    ret.append((s, COST[amphipod]))
        return ret

    def is_solved(self):
        return self.amphipods == {
            (2, 3): "A", (3, 3): "A",
            (2, 5): "B", (3, 5): "B",
            (2, 7): "C", (3, 7): "C",
            (2, 9): "D", (3, 9): "D",
        }

    def at(self, row, col):
        return self.amphipods.get((row, col), None)

    def heuristic(self):
        ret = 0
        if self.at(3, 3) == "A":
            ret += 1
            if self.at(2, 3) == "A":
                ret += 1
        if self.at(3, 5) == "B":
            ret += 1
            if self.at(2, 5) == "B":
                ret += 1
        if self.at(3, 5) == "B":
            ret += 1
            if self.at(2, 5) == "B":
                ret += 1
        # TODO
        return 8 - ret

    def solve(self):
        q, visited = [(0, self)], {self}
        while q:
            print(q, len(visited))
            # cost, curr = q.pop(0)
            cost, curr = heappop(q)
            if curr.is_solved():
                return cost
            for child, ch_cost in curr.possible_moves():
                if child in visited:
                    continue
                visited.add(child)
                next_cost = cost + ch_cost
                # q.append((next_cost, child))
                heappush(q, (next_cost, child))
        return None


if __name__ == "__main__":
    correct = """
#############
#...........#
###A#B#D#C###
  #A#B#C#D#
  #########
    """
    ex1 = """
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
    """
    ex2 = """
#############
#....B...C..#
###A#.#.#D###
  #A#B#C#D#
  #########
    """
    s = State.from_str(ex2)
    print(s.solve())
