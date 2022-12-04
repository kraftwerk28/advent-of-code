import sys

class Board:
    def __init__(self, rows):
        self.rows = rows
        self.marked = set()
        self.last_marked = None

    def clone(self):
        return Board(self.rows)

    def mark(self, num):
        self.last_marked = num
        self.marked.add(num)

    def is_bingo(self):
        for row in self.rows:
            if all(n in self.marked for n in row):
                return True
        for col_i, _ in enumerate(self.rows[0]):
            col = [row[col_i] for row in self.rows]
            if all(n in self.marked for n in col):
                return True
        # diag = [self.rows[i][i] for i, _ in enumerate(self.rows)]
        # if all(n in self.marked for n in diag):
        #     return True
        # diag = [self.rows[-i-1][i] for i, _ in enumerate(self.rows)]
        # if all(n in self.marked for n in diag):
        #     return True
        return False

    def score(self):
        s = sum(n for row in self.rows for n in row if n not in self.marked)
        return self.last_marked * s


def part1(boards, nums):
    newboards = [b.clone() for b in boards]
    for n in nums:
        for board in newboards:
            board.mark(n)
            if board.is_bingo():
                print(board.score())
                return


def part2(boards, nums):
    newboards = [b.clone() for b in boards]
    for n in nums:
        for board in newboards:
            board.mark(n)
        nonwin_boards = [b for b in newboards if not b.is_bingo()]
        if not nonwin_boards:
            print(newboards[0].score())
            return
        newboards = nonwin_boards


if __name__ == "__main__":
    boards = []
    nums = [int(n) for n in sys.stdin.readline().split(",")]
    while True:
        sys.stdin.readline()
        rows = [
            [int(n) for n in line.strip().split()]
            for line in [sys.stdin.readline() for _ in range(5)]
        ]
        if not rows[0]:
            break
        boards.append(Board(rows))
    part1(boards, nums)
    part2(boards, nums)
