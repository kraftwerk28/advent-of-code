import sys
from itertools import takewhile

lines = sys.stdin.read().splitlines()
trees = [[int(x) for x in line] for line in lines if line]


def part_1(trees):
    result = 0
    result += 2 * len(trees)
    result += 2 * (len(trees[0]) - 2)
    for rowi in range(1, len(trees) - 1):
        for coli in range(1, len(trees[0]) - 1):
            tree_height = trees[rowi][coli]
            left_row = trees[rowi][:coli]
            right_row = trees[rowi][coli + 1:]
            top_row = [row[coli] for row in trees[:rowi]]
            bottom_row = [row[coli] for row in trees[rowi + 1:]]
            for siblings in [top_row, right_row, bottom_row, left_row]:
                if all(h < tree_height for h in siblings):
                    result += 1
                    break
    print(f"Part 1: {result}")


def visible_count(tree_height, siblings):
    result = 0
    for i in siblings:
        result += 1
        if i >= tree_height:
            return result
    return result


def part_2(trees):
    max_score = 0
    for rowi in range(1, len(trees) - 1):
        for coli in range(1, len(trees[0]) - 1):
            tree_height = trees[rowi][coli]
            left_row = trees[rowi][:coli]
            left_row.reverse()
            right_row = trees[rowi][coli + 1:]
            top_row = [row[coli] for row in trees[:rowi]]
            top_row.reverse()
            bottom_row = [row[coli] for row in trees[rowi + 1:]]
            tree_score = (
                visible_count(tree_height, top_row) *
                visible_count(tree_height, right_row) *
                visible_count(tree_height, bottom_row) *
                visible_count(tree_height, left_row)
            )
            if tree_score > max_score:
                max_score = tree_score
    print(f"Part 2: {max_score}")


part_1(trees)
part_2(trees)
