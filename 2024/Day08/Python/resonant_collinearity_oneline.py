# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    len(
                        set(
                            sum(
                                map(
                                    lambda p: list(
                                        filter(
                                            lambda c: (
                                                (
                                                    (abs(c[0]-p[0][0]) + abs(c[1]-p[0][1])) == 2 * (abs(c[0]-p[1][0]) + abs(c[1]-p[1][1]))
                                                ) or (
                                                    (abs(c[0]-p[1][0]) + abs(c[1]-p[1][1])) == 2 * (abs(c[0]-p[0][0]) + abs(c[1]-p[0][1]))
                                                )
                                            ) and (
                                                (c[0] - p[0][0]) * (c[1] - p[1][1]) == (c[0] - p[1][0]) * (c[1] - p[0][1])
                                            ),
                                            sum(
                                                map(
                                                    lambda y: list(
                                                        map(
                                                            lambda x: (x, y),
                                                            range(input[1]+1)
                                                        )
                                                    ),
                                                    range(input[2]+1)
                                                ),
                                                []
                                            )
                                        )
                                    ),
                                    input[0]
                                ),
                                []
                            )
                        )
                    )
                ),
                "Part 2: {}".format(
                    len(
                        set(
                            sum(
                                map(
                                    lambda p: list(
                                        filter(
                                            lambda c: (c[0] - p[0][0]) * (c[1] - p[1][1]) == (c[0] - p[1][0]) * (c[1] - p[0][1]),
                                            sum(
                                                map(
                                                    lambda y: list(
                                                        map(
                                                            lambda x: (x, y),
                                                            range(input[1]+1)
                                                        )
                                                    ),
                                                    range(input[2]+1)
                                                ),
                                                []
                                            )
                                        )
                                    ),
                                    input[0]
                                ),
                                []
                            )
                        )
                    )
                )
            ]
        )(
            (
                lambda xs: (
                    sum(
                        map(
                            lambda ps:     sum(
                                map(
                                    lambda p1: list(
                                        map(
                                            lambda p2: (p1, p2),
                                            filter(
                                                lambda p2: p2 != p1,
                                                ps
                                            )
                                        )
                                    ),
                                    ps
                                ),
                                []
                            ),
                            (
                                lambda r1: lambda xs: r1(r1, xs)
                            )(
                                lambda r1, xs: [] if xs == [] else [
                                    (
                                        lambda r2: lambda xs, k: r2(r2, xs, k)
                                    )(
                                        lambda r2, xs, k: [] if xs == [] or xs[0][0] != k else [xs[0][1]] + r2(
                                            r2,
                                            xs[1:],
                                            k
                                        )
                                    )(
                                        xs,
                                        xs[0][0]
                                    )
                                ] + r1(
                                    r1,
                                    (
                                        lambda r3: lambda xs, k: r3(r3, xs, k)
                                    )(
                                        lambda r3, xs, k: [] if xs == [] else xs if xs[0][0] != k else r3(
                                            r3,
                                            xs[1:],
                                            k
                                        )
                                    )
                                    (xs, xs[0][0])
                                )
                            )(
                                list(
                                    filter(
                                        lambda xs: xs[0] != '.',
                                        xs
                                    )
                                )
                            )
                        ),
                        []
                    ),
                    *map(
                        max,
                        *map(
                            lambda x: x[1],
                            xs
                        )
                    )
                )
            )(
                sorted(
                    sum(
                        map(
                            lambda lines: list(
                                map(
                                    lambda c: (c[1], (c[0], lines[0])),
                                    enumerate(
                                        lines[1]
                                    )
                                )
                            ),
                            enumerate(open("../input.txt").read().strip().splitlines())
                        ),
                        []
                    ),
                    key=lambda x: x[0]
                )
            )
        )
    )
)
