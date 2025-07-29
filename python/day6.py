import sys


def is_guard(ch) -> bool:
    return ch in ('^', '>', '<', 'v')


def is_obstacle(pos, grid) -> bool:
    x, y = pos
    return grid[x][y] == '#'


def rotate_guard(ch):
    if ch == '^':
        return '>'
    elif ch == '>':
        return 'v'
    elif ch == 'v':
        return '<'
    elif ch == '<':
        return '^'


def is_leaving(pos: tuple[int, int], grid: list[str]) -> bool:
    x, y = pos
    guard: str = grid[x][y]

    return (guard == '^' and x == 0) or (guard == 'v' and x == len(grid) - 1) or (guard == '<' and y == 0) or (guard == '>' and y == len(grid[0]) - 1)


class GridMap:
    def __init__(self, grid: list[str]):
        self._grid = [list(s) for s in grid]
        self._guard_pos = next((x, y) for x, row in enumerate(grid) for y, tile in enumerate(row) if is_guard(tile))

    def _update_guard(self):
        x, y = self._guard_pos
        guard_heading = self._grid[x][y]

        headings = ['^', '>', 'v', '<']
        possible_moves = [(x - 1, y), (x, y + 1), (x + 1, y), (x, y - 1)]

        for heading, move in zip(headings, possible_moves):
            if guard_heading == heading:
                if is_obstacle(move, self._grid):
                    self._grid[x][y] = rotate_guard(guard_heading)
                else:
                    self._grid[x][y] = 'X'
                    x, y = move
                    self._grid[x][y] = guard_heading
                    self._guard_pos = move
                return

    def advance(self, verbose=False) -> int:
        if verbose:
            print(self._grid)

        while not is_leaving(self._guard_pos, self._grid):
            self._update_guard()
            if verbose:
                print(self._grid)

        x, y = self._guard_pos
        self._grid[x][y] = 'X'

        return sum(row.count('X') for row in self._grid)


def main():
    with open(sys.argv[1], 'r') as input_file:
        grid = input_file.read().splitlines()
        grid_map = GridMap(grid)
        print(grid_map.advance())


if __name__ == '__main__':
    main()
