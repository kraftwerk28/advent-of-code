import re
import sys


class Valves:
    def __init__(self, routes, flowrates, hero_at, time_passed=0):
        self.routes = routes
        self.flowrates = flowrates
        self.hero_at = hero_at
        self.opened = set()
        self.time_passed = time_passed

    time_limit = 30
    line_re = re.compile(
        r"Valve (\S+) has flow rate=([^;]+); tunnels? leads? to valves? (.+)")

    def shortest_path_to_valve(self, valve):
        dist = {v: 0xffff for v in self.routes.keys()}
        prev = {}
        dist[self.hero_at] = 0
        unvisited = set(dist)
        while True:
            cur = min(unvisited, key=lambda k: dist[k])
            unvisited.remove(cur)
            cur_cost = dist[cur]
            if cur == valve:
                break
            for near in self.routes[cur]:
                alt = cur_cost + 1
                if alt < dist[near]:
                    prev[near] = cur
                    dist[near] = alt
        return cur_cost, self.build_path(self.hero_at, valve, prev)

    def build_path(self, start, end, routes):
        path = [end]
        while path[0] != start:
            path.insert(0, routes[path[0]])
        return path

    def open_valve(self, valve):
        cost, _ = self.shortest_path_to_valve(valve)
        return Valves(self.routes, self.flowrates, valve, self.time_passed + cost + 1)

    @classmethod
    def from_lines(self, lines):
        routes = {}
        flowrates = {}
        hero_at = None
        for line in lines:
            m = re.match(Valves.line_re, line)
            from_valve = m[1]
            if hero_at is None:
                hero_at = from_valve
            dest_valves = m[3].split(", ")
            flowrates[from_valve] = int(m[2])
            routes[from_valve] = dest_valves
        return self(routes, flowrates, hero_at)


valves = Valves.from_lines(sys.stdin)
print(valves.routes, valves.flowrates, valves.hero_at)
print(valves.shortest_path_to_valve("JJ"))
