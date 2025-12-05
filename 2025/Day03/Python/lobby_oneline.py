# type: ignore
print(
    "\n".join(
        (
            lambda input: map(
                lambda x: x[0].format(x[1]),
                zip(
                    ["Part 1: {}", "Part 2: {}"],
                    map(
                        lambda x: sum(
                            map(
                                lambda xs: (
                                    lambda r: lambda xs, x, i, j, ans: r(r, xs, x, i, j, ans)
                                )(
                                    lambda r, xs, x, i, j, ans: ans if i == x else r(
                                        r,
                                        xs,
                                        x,
                                        i + 1,
                                        *max(
                                            map(
                                                lambda x: (
                                                    x[0] + j + 1,
                                                    ans * 10 + x[1]
                                                ),
                                                enumerate(xs[j:(len(xs)-x + 1 + i)]),
                                            ),
                                            key=lambda x: x[1]
                                        )
                                    )
                                )(xs, x, 0, 0, 0),
                                input,
                            )
                        ),
                        [2, 12]
                    )
                )
            )
        )(
            list(
                map(
                    lambda line: list(
                        map(
                            int,
                            line
                        )
                    ),
                    open("../input.txt").read().strip().splitlines()
                )
            )
        )
    )
)
