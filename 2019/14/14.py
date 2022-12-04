# import sys
# with open(sys.argv[1]) as f:
#     lines = f.read().splitlines()

lines = """
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
""".strip().splitlines()

lines = """
9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL
""".strip().splitlines()

lines = """
157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
""".strip().splitlines()

def parse_quantity(s):
    amount, name = s.split(" ")
    return int(amount), name


recipes = []
for line in lines:
    l, r = line.split(" => ")
    ll = l.split(", ")
    _from = parse_quantity(r)
    _to = [parse_quantity(it) for it in l.split(", ")]
    recipes.append((_from, _to))
print(recipes)

remaining = {}
total_used = {}


def get_ore_amount_for_name(name, amount):
    total_used[name] = total_used.get(name, 0) + amount
    if name == "ORE":
        return amount
    (outcome, _), ingredients = next(r for r in recipes if r[0][1] == name)
    existing = remaining.get(name, 0)
    if existing >= amount:
        remaining[name] = existing - amount
        return 0
    need_to_make = amount - existing
    if need_to_make <= outcome:
        remaining[name] = outcome - need_to_make
        reaction_count = 1
    else:
        if need_to_make % outcome == 0:
            remaining.pop(name, None)
            reaction_count = need_to_make // outcome
        else:
            a = (need_to_make // outcome) + 1
            remaining[name] = a*outcome % need_to_make
            reaction_count = a
    print(f"need {amount} of {name}; existing: {existing}; {reaction_count=}; remaining: {remaining.get(name,None)}")
    result = 0
    for need_amount, ing_name in ingredients:
        result += get_ore_amount_for_name(ing_name, reaction_count * need_amount)
    return result


result = get_ore_amount_for_name("FUEL", 1)
for k, v in total_used.items():
    print(k, v)
print(result)
