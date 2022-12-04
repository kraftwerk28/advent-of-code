from typing import List
import z3
import sys


class Alu:
    def __init__(self):
        self.regs = {}

    def __setitem__(self, k, v):
        try:
            self.regs[k] = int(v)
        except Exception:
            self.regs[k] = v

    def __getitem__(self, k):
        try:
            return int(k)
        except Exception:
            return self.regs.get(k, 0)

    def run(self, program: str, input: List[int]):
        for line in program.strip().splitlines():
            match line.split():
                case "inp", name:
                    self[name] = input.pop(0)
                case "add", a, b:
                    self[a] = self[a] + self[b]
                case "mul", a, b:
                    self[a] = self[a] * self[b]
                case "div", a, b:
                    self[a] = self[a] // self[b]
                case "mod", a, b:
                    self[a] = self[a] % self[b]
                case "eql", a, b:
                    self[a] = 1 if self[a] == self[b] else 0

    def __repr__(self):
        return (
            f"w = {self['w']};\tx = {self['x']};\t"
            f"y = {self['y']};\tz = {self['z']}"
        )


# mul x 0
# add x z
# mod x 26
# div z a
# add x b
# eql x w
# eql x 0
# mul y 0
# add y 25
# mul y x
# add y 1
# mul z y
# mul y 0
# add y w
# add y c
# mul y x
# add z y

def znext(nth, z0, z1, w, a, b, c):
    xt = z3.Int(f"xt_{nth}")
    return [
        # z3.If(z0 % 26 + b == w, xt == 0, xt == 1),
        xt == z3.If(z0 % 26 + b == w, 0, 1),
        z1 == (z0 / a) * (25 * xt + 1) + xt * (w + c),
    ]


if __name__ == "__main__":
    # s = z3.Optimize()
    # x, y, z = z3.Ints("x y z")
    # sol = s.maximize(z)
    # s += [z == x + y, x - y == 4, z < 10]
    # print(s)
    # s.check()
    # m = s.model()

    with open("input/24.txt") as f:
        lines = [line for line in f.read().splitlines() if line.strip()]
        chunks = [lines[i:i + 18] for i in range(0, len(lines), 18)]

    # for ch in chunks:
    #     print(ch[4])
    #     print(ch[5])
    #     print(ch[15])
    #     print()

    # for i, line in enumerate(zip(*chunks)):
    #     if not all(stmt == line[0] for stmt in line):
    #         print(f"#{i}: {set(line)}")

    input_digits = [z3.Int(f"input_{i}") for i in range(14)]
    input_as_num = z3.Int("monad")
    s = z3.Optimize()
    digits_ast = sum(n * 10**i for i, n in enumerate(input_digits[::-1]))
    s.add(input_as_num == digits_ast)
    s.add([n > 0 for n in input_digits])
    s.add([n < 10 for n in input_digits])
    z_chain = [0, *(z3.Int(f"z_{i}") for i in range(1, 15))]
    for i, (chunk, input_digit) in enumerate(zip(chunks, input_digits)):
        a = int(chunk[4].split()[2])
        b = int(chunk[5].split()[2])
        c = int(chunk[15].split()[2])
        s.add(znext(i, z_chain[i], z_chain[i+1], input_digit, a, b, c))
    s.add(z_chain[-1] == 0)
    s.minimize(input_as_num)
    print(s)
    print(s.check())
    m = s.model()
    print(m[input_as_num])
