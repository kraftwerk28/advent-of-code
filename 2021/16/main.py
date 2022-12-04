from dataclasses import dataclass
from typing import List
from functools import reduce
from operator import mul
import sys


@dataclass
class Packet:
    type: str
    ver: int
    len: int = None
    literal: int = None
    typeid: int = None
    subpackets: List["Packet"] = None


def tonumber(bits):
    return int(''.join(str(n) for n in bits), 2)


def parse_literal_packet(bits):
    ver = tonumber(bits[:3])
    ptr = 6
    numbits = []
    while True:
        is_last = bits[ptr] == 0
        numbits += bits[ptr+1:ptr+5]
        ptr += 5
        if is_last:
            break
    # Turns out I don't need to manual pad it :^)
    # PADSIZE = 4
    # if ptr % PADSIZE > 0:
    #     ptr += PADSIZE - (ptr % PADSIZE)
    p = Packet("literal", ver, typeid=4, len=ptr, literal=tonumber(numbits))
    return p, bits[ptr:]


def parse_operator_packet(bits):
    ver = tonumber(bits[:3])
    typeid = tonumber(bits[3:6])
    ptr = 6
    len_typeid = bits[ptr]
    ptr += 1
    subpackets = []
    p = Packet("operator", ver=ver, typeid=typeid, subpackets=subpackets)
    if len_typeid == 0:
        content_len = tonumber(bits[ptr:ptr+15])
        ptr += 15
        rest = bits[ptr:]
        while ptr - (6+1+15) < content_len:
            sp, rest = parse_packet(rest)
            subpackets.append(sp)
            ptr += sp.len
        p.len = ptr
        return p, bits[ptr:]
    elif len_typeid == 1:
        content_npacket = tonumber(bits[ptr:ptr+11])
        ptr += 11
        rest = bits[ptr:]
        while len(subpackets) < content_npacket:
            sp, rest = parse_packet(rest)
            subpackets.append(sp)
            ptr += sp.len
        p.len = ptr
        return p, bits[ptr:]


def parse_packet(bits):
    typeid = tonumber(bits[3:6])
    if typeid == 4:
        return parse_literal_packet(bits)
    else:
        return parse_operator_packet(bits)


def part1(packet: Packet):
    result = packet.ver
    if packet.type == "operator":
        result += sum(part1(p) for p in packet.subpackets)
    return result


def part2(packet: Packet):
    if packet.type == "literal":
        return packet.literal
    elif packet.type == "operator":
        if packet.typeid == 0:
            return sum(part2(p) for p in packet.subpackets)
        elif packet.typeid == 1:
            return reduce(mul, (part2(p) for p in packet.subpackets))
        elif packet.typeid == 2:
            return min(part2(p) for p in packet.subpackets)
        elif packet.typeid == 3:
            return max(part2(p) for p in packet.subpackets)
        elif packet.typeid == 5:
            a = part2(packet.subpackets[0])
            b = part2(packet.subpackets[1])
            return 1 if a > b else 0
        elif packet.typeid == 6:
            a = part2(packet.subpackets[0])
            b = part2(packet.subpackets[1])
            return 1 if a < b else 0
        elif packet.typeid == 7:
            a = part2(packet.subpackets[0])
            b = part2(packet.subpackets[1])
            return 1 if a == b else 0


if __name__ == "__main__":
    bits = []
    for x in sys.stdin.read().strip():
        n = int(x, 16)
        bits.append((n >> 3) & 1)
        bits.append((n >> 2) & 1)
        bits.append((n >> 1) & 1)
        bits.append(n & 1)
    p, rest = parse_packet(bits)
    print(f"part 1: {part1(p)}")
    print(f"part 2: {part2(p)}")
