# Enums
X = 0
Y = 1
Z = 2
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

    def __add__(self, v):
        x, y, z = self
        xx, yy, zz = v
        return Vec3(x+xx, y+yy, z+zz)

    def __sub__(self, v):
        x, y, z = self
        xx, yy, zz = v
        return Vec3(x-xx, y-yy, z-zz)

    def rotate(self, axis, dir):
        x, y, z = self
        if axis == X:
            if dir == CLOCKW:
                return Vec3(x, -z, y)
            elif dir == ANTICW:
                return Vec3(x, z, -y)
            elif dir == DOUBLE:
                return Vec3(x, -y, -z)
        elif axis == Y:
            if dir == CLOCKW:
                return Vec3(-z, y, x)
            elif dir == ANTICW:
                return Vec3(z, y, -x)
            elif dir == DOUBLE:
                return Vec3(-x, y, -z)
        elif axis == Z:
            if dir == CLOCKW:
                return Vec3(y, -x, z)
            elif dir == ANTICW:
                return Vec3(-y, x, z)
            elif dir == DOUBLE:
                return Vec3(-x, -y, z)

    def __repr__(self):
        return f"Vec3{super().__repr__()}"


class Cluster:
    def __init__(self, beacons):
        self.beacons = beacons

    def rotations(self):
        yield Cluster(self.beacons)
        yield self.rotate(X, CLOCKW)
        yield self.rotate(X, ANTICW)
        yield self.rotate(X, DOUBLE)

        yield self.rotate(Z, CLOCKW)
        yield self.rotate(Z, CLOCKW).rotate(Y, CLOCKW)
        yield self.rotate(Z, CLOCKW).rotate(Y, ANTICW)
        yield self.rotate(Z, CLOCKW).rotate(Y, DOUBLE)

        yield self.rotate(Z, ANTICW)
        yield self.rotate(Z, ANTICW).rotate(Y, CLOCKW)
        yield self.rotate(Z, ANTICW).rotate(Y, ANTICW)
        yield self.rotate(Z, ANTICW).rotate(Y, DOUBLE)

        yield self.rotate(Z, DOUBLE)
        yield self.rotate(Z, DOUBLE).rotate(X, CLOCKW)
        yield self.rotate(Z, DOUBLE).rotate(X, ANTICW)
        yield self.rotate(Z, DOUBLE).rotate(X, DOUBLE)

        yield self.rotate(Y, CLOCKW)
        yield self.rotate(Y, CLOCKW).rotate(Z, CLOCKW)
        yield self.rotate(Y, CLOCKW).rotate(Z, ANTICW)
        yield self.rotate(Y, CLOCKW).rotate(Z, DOUBLE)

        yield self.rotate(Y, ANTICW)
        yield self.rotate(Y, ANTICW).rotate(Z, CLOCKW)
        yield self.rotate(Y, ANTICW).rotate(Z, ANTICW)
        yield self.rotate(Y, ANTICW).rotate(Z, DOUBLE)

    def rotate(self, axis, dir):
        return Cluster([b.rotate(axis, dir) for b in self.beacons])

    def move(self, v: Vec3):
        return Cluster([b + v for b in self.beacons])

    def matching_beacons(self, o: "Cluster"):
        return set(self.beacons).intersection(set(o.beacons))


def part1(report):
    base_cluster = report[0]
    for rep in list(report.values())[1:]:
        for rotated in rep.rotations():
            for b in base_cluster.beacons:
                for b2 in rotated.beacons:
                    delta: Vec3 = b2 - b
                    matching = rotated.move(
                        delta).matching_beacons(base_cluster)
                    if len(matching) > 1:
                        print("Bingo")
                        return 0
    # for rotated_cluster in cluster2.rotations():
    # for scanner_index, beacons in list(report.items())[1:]:
    #     pass
    return 0


def part2():
    pass


if __name__ == "__main__":
    with open("input/19-sample.txt") as f:
        report = {}
        for sc_report in f.read().split("\n\n"):
            lines = sc_report.strip().splitlines()
            nth = int(lines[0].split()[2])
            beacons = [Vec3(*map(int, line.split(","))) for line in lines[1:]]
            report[nth] = Cluster(beacons)

    print(f"part 1: {part1(report)}")
    # print(f"part 2: {part2(inputs)}")
