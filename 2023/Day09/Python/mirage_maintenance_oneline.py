# type: ignore

print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    # input
                    sum(
                        map(
                            (lambda r: lambda seq: r(r, seq))(
                                lambda r, seq: 0
                                if all(map(lambda x: x == 0, seq))
                                else seq[-1]
                                + r(r, list(map(lambda a, b: b - a, seq, seq[1:])))
                            ),
                            input,
                        )
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            (lambda r: lambda seq: r(r, seq))(
                                lambda r, seq: 0
                                if all(map(lambda x: x == 0, seq))
                                else seq[0]
                                - r(r, list(map(lambda a, b: b - a, seq, seq[1:])))
                            ),
                            input,
                        )
                    )
                ),
            ]
        )(
            list(
                map(
                    lambda l: list(map(int, l.split())),
                    open("../input.txt").read().strip().splitlines(),
                )
            )
        )
    )
)
