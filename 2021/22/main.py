from dataclasses import dataclass
from typing import Tuple
import re
import sys


@dataclass
class Cuboid:
    p1: Tuple[int, int, int]
    p2: Tuple[int, int, int]

    def normalize(self):
        x1, y1, z1 = self.p1
        x2, y2, z2 = self.p2
        return Cuboid(
            (min(x1, x2), min(y1, y2), min(z1, z2)),
            (max(x1, x2), max(y1, y2), max(z1, z2)),
        )

    def volume(self):
        (x1, y1, z1), (x2, y2, z2) = self
        return abs(x1 - x2) * abs(y1 - y2) * abs(z1 - z2)

    def overlap(self, other: "Cuboid"):
        (x11, y11, z11), (x12, y12, z12) = self.normalize()
        (x21, y21, z21), (x22, y22, z22) = other.normalize()
        ix1, iy1, iz1 = max(x11, x21), max(y11, y21), max(z11, z21)
        ix2, iy2, iz2 = min(x12, x22), min(y12, y22), min(z12, z22)
        if ix1 > ix2 or iy1 > iy2 or iz1 > iz2:
            return None
        else:
            return Cuboid((ix1, iy1, iz1), (ix2, iy2, iz2))

    def __iter__(self):
        yield self.p1
        yield self.p2

    @staticmethod
    def from_str(s):
        x1, x2, y1, y2, z1, z2 = [int(n) for n in re.findall(r"-?\d+", s)]
        return Cuboid((x1, y1, z1), (x2+1, y2+1, z2+1))


def reboot(input, with_clamp):
    if with_clamp:
        clamper = Cuboid((-50, -50, -50), (51, 51, 51))
        input = (
            (op, c.overlap(clamper))
            for op, c in input if c.overlap(clamper)
        )
    cuboids_on, cuboids_off = [], []
    for op, cuboid in input:
        cuboids_on1, cuboids_off1 = cuboids_on[:], cuboids_off[:]
        if op == "off":
            for prior_cuboid in cuboids_on:
                if (overl := cuboid.overlap(prior_cuboid)) is not None:
                    cuboids_off1.append(overl)
            for prior_cuboid in cuboids_off:
                if (overl := cuboid.overlap(prior_cuboid)) is not None:
                    cuboids_on1.append(overl)
        elif op == "on":
            for prior_cuboid in cuboids_on:
                if (overl := cuboid.overlap(prior_cuboid)) is not None:
                    cuboids_off1.append(overl)
            for prior_cuboid in cuboids_off:
                if (overl := cuboid.overlap(prior_cuboid)) is not None:
                    cuboids_on1.append(overl)
            cuboids_on1.append(cuboid)
        cuboids_on, cuboids_off = cuboids_on1, cuboids_off1
    return (
        sum(c.volume() for c in cuboids_on) -
        sum(c.volume() for c in cuboids_off)
    )


if __name__ == '__main__':
    input = []
    for line in sys.stdin.read().splitlines():
        if not line:
            continue
        a, b = line.split()
        input.append((a, Cuboid.from_str(b)))
    print(f"part 1: {reboot(input, True)}")
    print(f"part 2: {reboot(input, False)}")
