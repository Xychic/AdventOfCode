# type: ignore
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
                                        (lambda r: lambda xs, acc: r(r, xs, acc))(
                                            lambda r, xs, acc: acc
                                            if not xs
                                            else r(r, xs[1:], acc.replace(xs[0][0], xs[0][1]))
                                        )(
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
