import sys
from dataclasses import dataclass, field

Bill = list[tuple[str, int]]


@dataclass
class Factory:
    blueprint: dict[str, Bill] = field(repr=False)
    minute: int = 0
    resources: dict[str, int] = field(default_factory=lambda: {})
    robots: dict[str, int] = field(default_factory=lambda: {"ore": 1})

    def evaluate(self):
        if self.minute == 24:
            return self.resources.get("geode", 0)
        self.collect_resources()
        eval_results = [self.evaluate()]
            # print("Branching from", self)
            # print(branches)
            # return max(branch.evaluate() for branch in branches)
        eval_results.extend(branch.evaluate() for branch in self.available_spends())
        # else:
        #     print("Collecting")
        #     self.collect_resources()
        #     return self.evaluate()
        return max(eval_results)

    def collect_resources(self):
        for robo, count in self.robots.items():
            self.resources[robo] = self.resources.get(robo, 0) + count
        print(self)
        self.minute += 1

    def copy_with_exchange(self, buy_robot: str) -> "Factory":
        bill = self.blueprint[buy_robot]
        new_resources = self.resources.copy()
        for res, amount in bill:
            avail = self.resources.get(res, 0)
            if avail < amount:
                return None
            new_resources[res] -= amount
        new_robots = self.robots.copy()
        new_robots[buy_robot] = self.robots.get(buy_robot, 0) + 1
        return Factory(self.blueprint, self.minute, new_resources, new_robots)

    def available_spends(self):
        result = []
        for robo in self.blueprint:
            if (new_factory := self.copy_with_exchange(robo)) is not None:
                result.append(new_factory)
        return result


blueprints = {}
for line in sys.stdin:
    parts = line.split(":")
    bp_id = int(parts[0].split()[1])
    bp_data: dict[str, Bill] = {}
    for robo in parts[1].split("."):
        parts = robo.strip().split()
        if len(parts) == 6:
            robo_type = parts[1]
            bill = [(parts[-1], int(parts[-2]))]
        elif len(parts) == 9:
            robo_type = parts[1]
            bill = [(parts[-1], int(parts[-2])), (parts[-4], int(parts[-5]))]
        bp_data[robo_type] = bill
    blueprints[bp_id] = bp_data

for k, v in blueprints.items():
    print(v)

f = Factory(blueprints[1])
# f.resources.update({"ore": 4, "clay": 14})
# print(f.copy_with_exchange("obsidian"))
print(f.evaluate())
