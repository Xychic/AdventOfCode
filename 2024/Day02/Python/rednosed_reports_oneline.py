# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    len(
                        list(
                            filter(
                                lambda xs: (
                                        xs == sorted(xs) or
                                        xs == sorted(xs, reverse=True)
                                    ) and (
                                        all(
                                            map(
                                                lambda a, b: 1 <= abs(a-b) <= 3,
                                                xs,
                                                xs[1:]
                                            )
                                        )
                                    ),
                                input
                            )
                        )
                    )
                ),
                "Part 2: {}".format(
                    len(
                        list(
                            filter(
                                lambda line: any(
                                    map(
                                        lambda xs: (
                                            xs == sorted(xs) or
                                            xs == sorted(xs, reverse=True)
                                        ) and (
                                            all(
                                                map(
                                                    lambda a, b: 1 <= abs(a-b) <= 3,
                                                    xs,
                                                    xs[1:]
                                                )
                                            )
                                        ),
                                        map(
                                            lambda i: line[:i] + line[i+1:],
                                            range(0, len(line))
                                        )
                                    )
                                ),
                                input
                            )
                        )
                    )
                )
            ]
        )(
            list(
                map(
                    lambda line: list(
                        map(
                            int,
                            line.split()
                        )
                    ),
                    open("../input.txt").read().strip().splitlines()
                )
            )
        )
    )
)
