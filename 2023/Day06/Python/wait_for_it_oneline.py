# type: ignore

print(
    "\n".join(
        (
            lambda input, solve: [
                "Part 1: {}".format(
                    (lambda r: lambda acc, xs: r(r, acc, xs))(
                        lambda r, acc, xs: acc if not xs else r(r, acc * xs[0], xs[1:])
                    )(1, list(map(solve, input)))
                ),
                "Part 2: {}".format(
                    solve(
                        (lambda r: lambda acc_a, acc_b, xs: r(r, acc_a, acc_b, xs))(
                            lambda r, acc_a, acc_b, xs: (acc_a, acc_b)
                            if not xs
                            else r(r, acc_a + xs[0][0], acc_b + xs[0][1], xs[1:])
                        )("", "", input)
                    )
                ),
            ]
        )(
            list(
                zip(
                    *map(
                        lambda l: l.split(),
                        open("../input.txt").read().strip().splitlines(),
                    )
                )
            )[1:],
            lambda x: int(
                (
                    (
                        (int(x[0]) + (int(x[0]) ** 2 - (4 * int(x[1]))) ** 0.5) / 2
                        - 0.001
                    )
                    // 1
                )
                - (
                    (
                        (int(x[0]) - (int(x[0]) ** 2 - (4 * int(x[1]))) ** 0.5) / 2
                        + 0.001
                    )
                    // 1
                )
            ),
        )
    )
)
