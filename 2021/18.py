from itertools import permutations
from dataclasses import dataclass


@dataclass
class Node:
    value: int = None
    parent: "Node" = None
    left: "Node" = None
    right: "Node" = None

    def explode(self):
        assert self.left is not None and self.left.value is not None
        assert self.right is not None and self.right.value is not None
        if (left_sib := self.find_left_sibling()) is not None:
            left_sib.value += self.left.value
        if (right_sib := self.find_right_sibling()) is not None:
            right_sib.value += self.right.value
        self.value = 0
        self.left = None
        self.right = None

    def split(self):
        a, b = self.value//2, self.value//2
        if self.value % 2 != 0:
            b += 1
        self.left = Node(value=a, parent=self)
        self.right = Node(value=b, parent=self)
        self.value = None

    def find_left_sibling(self):
        c = self
        while c.parent and c.parent.left is c:
            c = c.parent
        if c.parent is None:
            return None
        c = c.parent.left
        while c.right is not None:
            c = c.right
        return c

    def find_right_sibling(self):
        c = self
        while c.parent and c.parent.right is c:
            c = c.parent
        if c.parent is None:
            return None
        c = c.parent.right
        while c.left is not None:
            c = c.left
        return c

    def magnitude(self):
        if self.value is not None:
            return self.value
        return self.left.magnitude()*3 + self.right.magnitude()*2

    def try_explode(self, depth=0):
        if depth >= 4 and self.value is None:
            self.explode()
            return True
        if self.left is not None and self.left.try_explode(depth=depth+1):
            return True
        if self.right is not None and self.right.try_explode(depth=depth+1):
            return True
        return False

    def try_split(self):
        if self.value is not None and self.value > 9:
            self.split()
            return True
        return (
            (self.left is not None and self.left.try_split())
            or (self.right is not None and self.right.try_split())
        )

    def reduce_once(self):
        return self.try_explode() or self.try_split()

    def reduce(self):
        while self.reduce_once():
            pass
        return self

    def to_list(self):
        if self.value is not None:
            return self.value
        return [self.left.to_list(), self.right.to_list()]

    def __repr__(self):
        if self.value is not None:
            return repr(self.value)
        return f"[{self.left}, {self.right}]"

    @staticmethod
    def from_list(v, parent=None):
        n = Node(parent=parent)
        if type(v) is int:
            n.value = v
        else:
            n.left = Node.from_list(v[0], n)
            n.right = Node.from_list(v[1], n)
        return n


def part1(inputs):
    result = Node.from_list(inputs[0])
    for arr in inputs[1:]:
        n = Node.from_list([result.to_list(), arr])
        n.reduce()
        result = n
    return result.magnitude()


def part2(inputs):
    return max(
        Node.from_list(p).reduce().magnitude()
        for p in permutations(inputs, 2)
    )


if __name__ == "__main__":
    with open("input/18.txt") as f:
        inputs = []
        for line in f:
            inputs.append(eval(line))
    print(f"part 1: {part1(inputs)}")
    print(f"part 2: {part2(inputs)}")
