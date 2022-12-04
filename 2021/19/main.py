import itertools
from typing import Dict, Tuple, Set
from enum import Enum
import sys


class Axis(Enum):
    X = 0
    Y = 1
    Z = 2


class Dir(Enum):
    CLOCKW = 3
    ANTICW = 4
    DOUBLE = 5


#       Z
#       |
#       |
#      / - - - Y
#    /
#  X


class Vec3(tuple):
    def __new__(self, *args):
        return super().__new__(Vec3, args)

    def __hash__(self):
        t = tuple(x for x in self)
        return hash(t)

    def __add__(self, v):
        x, y, z = self
        xx, yy, zz = v
        return Vec3(x+xx, y+yy, z+zz)

    def __neg__(self):
        return Vec3(*(-n for n in self))

    def __sub__(self, v):
        x, y, z = self
        xx, yy, zz = v
        return Vec3(x-xx, y-yy, z-zz)

    def rotate(self, axis, dir):
        x, y, z = self
        if axis == Axis.X:
            if dir == Dir.CLOCKW:
                return Vec3(x, -z, y)
            elif dir == Dir.ANTICW:
                return Vec3(x, z, -y)
            elif dir == Dir.DOUBLE:
                return Vec3(x, -y, -z)
        elif axis == Axis.Y:
            if dir == Dir.CLOCKW:
                return Vec3(-z, y, x)
            elif dir == Dir.ANTICW:
                return Vec3(z, y, -x)
            elif dir == Dir.DOUBLE:
                return Vec3(-x, y, -z)
        elif axis == Axis.Z:
            if dir == Dir.CLOCKW:
                return Vec3(y, -x, z)
            elif dir == Dir.ANTICW:
                return Vec3(-y, x, z)
            elif dir == Dir.DOUBLE:
                return Vec3(-x, -y, z)

    @classmethod
    def from_str(cls, s: str) -> "Vec3":
        return cls(*(int(n) for n in s.split(",")))

    def __eq__(self, v: "Vec3"):
        return all(a == b for a, b in zip(self, v))

    def __repr__(self):
        x, y, z = self
        return f"Vec3({x:5},{y:5},{z:5})"

    def sqd(self, v: "Vec3") -> int:
        return sum((a-b)**2 for a, b in zip(self, v))

    def manhattan_d(self, v: "Vec3") -> int:
        return sum(abs(a - b) for a, b in zip(self, v))


class Cluster:
    def __init__(self, beacons):
        self.beacons = set(beacons)
        self.dis = set(
            a.sqd(b) for a, b
            in itertools.combinations(self.beacons, r=2)
        )
        assert len(self.beacons) > 0

    def __eq__(self, c: "Cluster"):
        return self.beacons == c.beacons

    def offset(self, v: Vec3) -> "Cluster":
        return Cluster(b + v for b in self.beacons)

    def align_index(self, c: "Cluster"):
        return len(self.dis & c.dis)

    def try_align(self, c: "Cluster") -> Tuple["Cluster", Set[Vec3], Vec3]:
        for r in c.rotations():
            for a, b in itertools.product(r.beacons, self.beacons):
                offset_vec = b - a
                new_cluster = r.offset(offset_vec)
                matched = self.beacons & new_cluster.beacons
                if len(matched) >= 12:
                    return new_cluster, matched, offset_vec
        return None, None, None

    def merge(self, c: "Cluster") -> "Cluster":
        return Cluster(self.beacons | c.beacons)

    def rotations(self):
        for f in [
            self,
            self.rotate(Axis.Z, Dir.CLOCKW),
            self.rotate(Axis.Z, Dir.DOUBLE),
            self.rotate(Axis.Z, Dir.ANTICW),
            self.rotate(Axis.Y, Dir.CLOCKW),
            self.rotate(Axis.Y, Dir.ANTICW),
        ]:
            yield f
            yield f.rotate(Axis.X, Dir.CLOCKW)
            yield f.rotate(Axis.X, Dir.ANTICW)
            yield f.rotate(Axis.X, Dir.DOUBLE)

    def rotate(self, axis, dir):
        # match axis, dir:
        #     case Axis.X, Dir.CLOCKW:
        #         return Cluster(Vec3(x, -z, y) for x, y, z in self.beacons)
        #     case Axis.X, Dir.ANTICW:
        #         return Cluster(Vec3(x, z, -y) for x, y, z in self.beacons)
        #     case Axis.X, Dir.DOUBLE:
        #         return Cluster(Vec3(x, -y, -z) for x, y, z in self.beacons)
        #     case Axis.Y, Dir.CLOCKW:
        #         return Cluster(Vec3(-z, y, x) for x, y, z in self.beacons)
        #     case Axis.Y, Dir.ANTICW:
        #         return Cluster(Vec3(z, y, -x) for x, y, z in self.beacons)
        #     case Axis.Y, Dir.DOUBLE:
        #         return Cluster(Vec3(-x, y, -z) for x, y, z in self.beacons)
        #     case Axis.Z, Dir.CLOCKW:
        #         return Cluster(Vec3(y, -x, z) for x, y, z in self.beacons)
        #     case Axis.Z, Dir.ANTICW:
        #         return Cluster(Vec3(-y, x, z) for x, y, z in self.beacons)
        #     case Axis.Z, Dir.DOUBLE:
        #         return Cluster(Vec3(-z, -y, z) for x, y, z in self.beacons)
        return Cluster(b.rotate(axis, dir) for b in self.beacons)


if __name__ == "__main__":
    clusters: Dict[int, Cluster] = {}
    for t in sys.stdin.read().split("\r\n\r\n"):
        label, *vecs = t.strip().split("\r\n")
        nscanner = int(label.split()[2])
        clusters[nscanner] = Cluster([Vec3.from_str(s) for s in vecs])

    p1clusters = clusters.copy()
    fst = p1clusters.pop(0)
    offsets = [Vec3(0, 0, 0)]
    while p1clusters:
        alignable = max(
            p1clusters,
            key=lambda k: fst.align_index(p1clusters[k]),
        )
        new_cluster, matched, offset = fst.try_align(p1clusters.pop(alignable))
        fst = fst.merge(new_cluster)
        offsets.append(offset)
        # print(f"{alignable:<3} aligned")

    print(f"Part 1: {len(fst.beacons)}")
    p2 = max(a.manhattan_d(b) for a, b in itertools.combinations(offsets, r=2))
    print(f"Part 2: {p2}")
