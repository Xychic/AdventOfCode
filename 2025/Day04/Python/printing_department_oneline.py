# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda x: sum(
                                map(
                                    lambda n: n in input,
                                    [
                                        (x[0] - 1, x[1] - 1),
                                        (x[0], x[1] - 1),
                                        (x[0] + 1, x[1] - 1),
                                        (x[0] - 1, x[1]),
                                        (x[0] + 1, x[1]),
                                        (x[0] - 1, x[1] + 1),
                                        (x[0], x[1] + 1),
                                        (x[0] + 1, x[1] + 1),
                                    ]
                                )
                            ) < 4,
                            input
                        )
                    )
                ),
                "Part 2: {}".format(
                    (
                        lambda r: lambda x: r(r, x)
                    )(
                        lambda r, x: (
                            lambda z: 0 if len(z) == 0 else len(z) + r(r, x ^ z)
                        )(
                            set(
                                filter(
                                    lambda y: sum(
                                        map(
                                            lambda n: n in x,
                                            [
                                                (y[0] - 1, y[1] - 1),
                                                (y[0], y[1] - 1),
                                                (y[0] + 1, y[1] - 1),
                                                (y[0] - 1, y[1]),
                                                (y[0] + 1, y[1]),
                                                (y[0] - 1, y[1] + 1),
                                                (y[0], y[1] + 1),
                                                (y[0] + 1, y[1] + 1),
                                            ]
                                        )
                                    ) < 4,
                                    x
                                )
                            )
                        )
                    )(input)
                )
            ]
        )(
            set(
                sum(
                    map(
                        lambda y_line: list(
                            map(
                                lambda x: (x[0], x[1]),
                                filter(
                                    lambda x: x[2] == '@',
                                    map(
                                        lambda x: (x[0], y_line[0], x[1]),
                                        enumerate(y_line[1])
                                    )
                                )
                            )
                        ),
                        enumerate(open("../input.txt").read().strip().splitlines()),
                    ),
                    []
                )
            )
        )
    )
)
