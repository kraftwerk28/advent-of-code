def minmax(image):
    return (
        (min(i for i, _ in image), max(i for i, _ in image)),
        (min(j for _, j in image), max(j for _, j in image)),
    )


def enhance(image, enhancement):
    pixels, outer_bit = image
    new_outer_bit = enhancement[-1] if outer_bit == 1 else enhancement[0]
    (min_i, max_i), (min_j, max_j) = minmax(pixels)
    if outer_bit == 1:
        pixels = pixels.copy()
        for i in range(min_i-2, max_i+3):
            pixels.add((i, min_j-1))
            pixels.add((i, min_j-2))
            pixels.add((i, max_j+1))
            pixels.add((i, max_j+2))
        for j in range(min_j, max_j+1):
            pixels.add((min_i-1, j))
            pixels.add((min_i-2, j))
            pixels.add((max_i+1, j))
            pixels.add((max_i+2, j))
    result = set()
    for i in range(min_i - 1, max_i + 2):
        for j in range(min_j - 1, max_j + 2):
            ppx = int("".join(
                "1" if (i+di, j+dj) in pixels else "0"
                for di in range(-1, 2) for dj in range(-1, 2)
            ), 2)
            if enhancement[ppx] == 1:
                result.add((i, j))
    return (result, new_outer_bit)


def render(image):
    (min_i, max_i), (min_j, max_j) = minmax(image)
    return "\n".join(
        "".join("#" if (i, j) in image else "." for j in range(min_j, max_j+1))
        for i in range(min_i, max_i+1)
    )


def part1(image, enhancement):
    image = enhance(image, enhancement)
    image = enhance(image, enhancement)
    return len(image[0])


def part2(image, enhancement):
    for _ in range(50):
        image = enhance(image, enhancement)
    return len(image[0])


if __name__ == "__main__":
    with open("input/20.txt") as f:
        image = (set(), 0)
        enh, img = f.read().split("\n\n", maxsplit=2)
        enhancement = [1 if c == "#" else 0 for c in enh]
        image = set()
        for i, line in enumerate(img.splitlines()):
            for j, c in enumerate(line):
                if c == "#":
                    image.add((i, j))
    print(f"part 1: {part1((image, 0), enhancement)}")
    print(f"part 2: {part2((image, 0), enhancement)}")
