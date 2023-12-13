# type: ignore

print(
    "\n".join(
        (
            lambda input, solve: [
                "Part 1: {}".format(sum(map(lambda b: solve(b, 0), input))),
                "Part 2: {}".format(sum(map(lambda b: solve(b, 1), input))),
            ]
        )(
            list(
                map(
                    lambda block: list(map(list, block.splitlines())),
                    open("../input.txt").read().strip().split("\n\n"),
                )
            ),
            lambda b, d: next(
                filter(
                    lambda x: sum(
                        map(
                            lambda lr: sum(
                                map(lambda row: (row[lr[0]] != row[lr[1]]), b)
                            ),
                            filter(
                                lambda lr: lr[0] >= 0,
                                map(
                                    lambda dx: (x - dx, x + 1 + dx),
                                    range(len(b[0]) - x - 1),
                                ),
                            ),
                        )
                    )
                    == d,
                    range(len(b[0]) - 1),
                ),
                100
                * (
                    next(
                        filter(
                            lambda y: sum(
                                map(
                                    lambda ud: sum(
                                        map(
                                            lambda x: b[ud[0]][x]
                                            != b[ud[1]][x],
                                            range(len(b[0])),
                                        )
                                    ),
                                    filter(
                                        lambda ud: ud[0] >= 0,
                                        map(
                                            lambda dy: (y - dy, y + 1 + dy),
                                            range(len(b) - y - 1),
                                        ),
                                    ),
                                )
                            )
                            == d,
                            range(len(b) - 1),
                        ),
                        -1,
                    )
                    + 1
                )
                - 1,
            )
            + 1,
        )
    )
)
