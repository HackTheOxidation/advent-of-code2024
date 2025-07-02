import itertools
import sys


def get_x_coords(lines):
    lines = [line.strip() for line in lines] 
    return (
        (i, j)
        for i, line in enumerate(lines)
        for j, ch in enumerate(line) 
        if ch == 'X'
    )
    

def get_moves():
    directions = (0, 1, -1)
    for move in itertools.product(directions, directions):
        if move != (0, 0):
            yield move


def traverse_and_count(lines) -> int:
    rows = len(lines)
    cols = len(lines[0])

    def _check_bounds(i, j):
        return (0 <= i < rows) and (0 <= j < cols)

    def _traverse(i, j, update_coords, count=3):
        char = lines[i][j]
        if count > 0 and (new_coords := update_coords(i, j)) and _check_bounds(*new_coords):
            return char + _traverse(*new_coords, update_coords, count - 1)
        return char 

    return sum(
        1 
        for i, j in get_x_coords(lines)
        for x, y in get_moves()
        if _traverse(i, j, lambda a, b: (a + x, b + y)) == 'XMAS'
    )


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        lines = f.readlines()
        xmas_count = traverse_and_count(lines)
        print(xmas_count)
