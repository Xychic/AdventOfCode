# type: ignore



print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda x: any(
                                map(
                                    lambda y: y[0] <= x <= y[1],
                                    input[0]
                                )
                            ),
                            input[1]
                        )
                    )
                ),
                "Part 2: {}".format(
                    (
                        lambda r: lambda xs, x: r(r, xs, x)
                    )(
                        lambda r, xs, x:
                            0
                                if xs == [] else
                            r(r, xs[1:], x)
                                if xs[0][1] <= x else
                            xs[0][1] - x + r(r, xs[1:], max(x, xs[0][1]))
                                if xs[0][0] <= x else
                            1+xs[0][1] - xs[0][0] + r(r, xs[1:], max(x, xs[0][1]))

                    )(
                        sorted(
                            input[0]
                        ),
                        0
                    )
                )
            ]
        )(
            (
                lambda xs, ys: (
                    list(
                        map(
                            lambda x: tuple(
                                map(
                                    int,
                                    x.split("-")
                                )
                            ),
                            xs.splitlines(),
                        )
                    ),
                    list(
                        map(
                            int,
                            ys.splitlines(),
                        )
                    )
                )
            )(
                *open("../input.txt").read().strip().split("\n\n")
            )
        )
    )
)
