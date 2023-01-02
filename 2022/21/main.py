import sys
import time
import operator as op

opdict = {
    "*": op.mul,
    "/": op.floordiv,
    "+": op.add,
    "-": op.sub,
}

monkes = {}
for line in sys.stdin:
    parts = line.split()
    lhs = parts[0][:-1]
    if len(parts) == 2:
        rhs = int(parts[1])
    else:
        rhs = (parts[1], opdict[parts[2]], parts[3])
    monkes[lhs] = rhs


def yell(monke):
    rhs = monkes[monke]
    if type(rhs) is int:
        return rhs
    else:
        a, op, b = rhs
        return op(yell(a), yell(b))


print(f"Part 1: {yell('root')}")


def yell2(monke, bisec):
    if monke == "humn":
        return bisec
    rhs = monkes[monke]
    if type(rhs) is int:
        return rhs
    else:
        a, op, b = rhs
        return op(yell2(a, bisec), yell2(b, bisec))


a, _, b = monkes["root"]
bisec = 50000000000000
frac = bisec // 2
while True:
    ansa = yell2(a, bisec)
    ansb = yell2(b, bisec)
    if ansa < ansb:
        bisec -= frac
    elif ansa > ansb:
        bisec += frac
    else:
        break
    frac //= 2

# There are multiple "correct" answers, let's pick the lowest
while yell2(a, bisec) == yell2(b, bisec):
    bisec -= 1
bisec += 1

print(f"Part 2: {bisec}")
