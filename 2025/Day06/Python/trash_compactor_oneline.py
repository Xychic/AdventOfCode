# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda x: x[0](x[1:]),
                            zip(
                                map(
                                    lambda c: sum if c == "+" else lambda x: (
                                        lambda r1: lambda xs: r1(r1, xs)
                                    )(
                                        lambda r1, xs: xs[0] if len(xs) == 1 else xs[0] * r1(r1, xs[1:])
                                    )(
                                        list(x)
                                    ),
                                    input[-1].split()
                                ),
                                *map(
                                    lambda l: map(
                                        int,
                                        l.split()
                                    ),
                                    input[:-1]
                                )
                            )
                        )
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda x: x[0](x[1]),
                            zip(
                                map(
                                    lambda c: sum if c == "+" else lambda x: (
                                        lambda r1: lambda xs: r1(r1, xs)
                                    )(
                                        lambda r1, xs: xs[0] if len(xs) == 1 else xs[0] * r1(r1, xs[1:])
                                    )(
                                        list(x)
                                    ),
                                    input[-1].split()
                                ),
                                map(
                                    lambda x: map(
                                        int,
                                        x.split(",")
                                    ),
                                    ",".join(
                                        list(
                                                map(
                                                    lambda x: "".join(x).strip(),
                                                    zip(
                                                        *map(
                                                            list,
                                                            input[:-1]
                                                        )
                                                    )
                                                )
                                            )
                                    ).split(",,")
                                )
                            )
                        )
                    )
                )
            ]
        )(
            open("../input.txt").read().splitlines()
        )
    )
)
