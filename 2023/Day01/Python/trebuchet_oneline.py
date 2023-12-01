print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda l: (lambda x: int(x[0] + x[-1]))(
                                list(filter(lambda c: c.isdigit(), l))
                            ),
                            input,
                        )
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda l: (lambda x: int(x[0] + x[-1]))(
                                list(
                                    filter(
                                        lambda c: c.isdigit(),
                                        (lambda r: lambda f, i, s: r(r, f, i, s))(
                                            lambda r, f, i, s: s
                                            if i == []
                                            else r(r, f, i[1:], f(s, i[0]))
                                        )(
                                            lambda x, y: x.replace(y[0], y[1]),
                                            [
                                                ("one", "o1e"),
                                                ("two", "t2o"),
                                                ("three", "t3e"),
                                                ("four", "f4r"),
                                                ("five", "f5e"),
                                                ("six", "s6x"),
                                                ("seven", "s7n"),
                                                ("eight", "e8t"),
                                                ("nine", "n9e"),
                                            ],
                                            l,
                                        ),
                                    )
                                )
                            ),
                            input,
                        ),
                    )
                ),
            ]
        )(open("../input.txt").read().strip().splitlines())
    )
)
