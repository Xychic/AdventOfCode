#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 python3Packages.sympy python3Packages.numpy
# type: ignore

import time
import sympy

class Timer:
    def __init__(self):
        self.start = time.time_ns()

    def elapsed(self):
        symbol = ["ns", "μs", "ms", "s"]
        index = 0
        elapsed = time.time_ns() - self.start
        while elapsed > 1000:
            elapsed /= 1000
            index += 1
        return f"{elapsed:,.0f}{symbol[index]}"


Input = list[tuple[tuple[int, int], tuple[int, int], tuple[int, int]]]


def main():
    start = Timer()
    raw_input = open(
        "../input.txt"
    ).read()
    input = parse(raw_input)
    print(f"Parsed input in {start.elapsed()}")
    start = Timer()
    print(f"Part 1: {part_1(input)}, took {start.elapsed()}")
    start = Timer()
    print(f"Part 2: {part_2(input)}, took {start.elapsed()}")


def parse(input: str) -> Input:
    return list(
        map(
            lambda line: tuple(
                zip(*(map(lambda x: tuple(map(int, x.split(", "))), line.split(" @ "))))
            ),
            input.splitlines(),
        )
    )


def part_1(input_: Input) -> int:
    MIN = 200_000_000_000_000
    MAX = 400_000_000_000_000

    answer = 0

    for i, ((xp1, xv1), (yp1, yv1), _) in enumerate(input_):
        for j, ((xp2, xv2), (yp2, yv2), _) in list(enumerate(input_))[i+1:]:
            try:
                t2 = (yp2 - yp1 + (yv1 / xv1) * xp1 - (yv1 / xv1) * xp2) / (
                    (yv1 / xv1) * xv2 - yv2
                )
            except ZeroDivisionError:
                continue
            t1 = (xp2 + xv2 * t2 - xp1) / xv1

            dest_x = xp1 + xv1 * t1
            dest_y = yp1 + yv1 * t1

            if t1 >= 0 and t2 >= 0 and MIN <= dest_x <= MAX and MIN <= dest_y <= MAX:
                # input(f"{i}: {j} ({dest_x}, {dest_y})")
                answer += 1

    return answer


def part_2(data: Input) -> int:
    xp = sympy.var("xp")
    yp = sympy.var("yp")
    zp = sympy.var("zp")

    xv = sympy.var("xv")
    yv = sympy.var("yv")
    zv = sympy.var("zv")

    equations = []

    for i, (x, y, z) in enumerate(data[:4]):
        t = sympy.var(f"t{i}")
        equations.append(sympy.Eq(xp + t * xv, x[0] + x[1] * t))
        equations.append(sympy.Eq(yp + t * yv, y[0] + y[1] * t))
        equations.append(sympy.Eq(zp + t * zv, z[0] + z[1] * t))

    intersection = sympy.solve(equations)[0]

    return sum(intersection[p] for p in (xp, yp, zp))


if __name__ == "__main__":
    main()
