def cost1(nums, target):
    return sum(abs(n - target) for n in nums)


def cost2(nums, target):
    sum = 0
    for n in nums:
        m = abs(n - target)
        sum += (m + m**2) // 2
    return sum


if __name__ == "__main__":
    with open("input/07.txt") as f:
        nums = [int(n) for n in f.read().strip().split(",")]
    p1 = min(cost1(nums, n) for n in range(min(nums), max(nums)+1))
    print(f"part 1: {p1}")
    p2 = min(cost2(nums, n) for n in range(min(nums), max(nums)+1))
    print(f"part 2: {p2}")
