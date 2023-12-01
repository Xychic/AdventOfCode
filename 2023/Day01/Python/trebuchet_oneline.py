print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        (lambda l: int(l[0] + l[-1]))(
                            list(filter(lambda c: c.isdigit(), l))
                        )
                        for l in input
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda l: (lambda x: x[0][1] * 10 + x[-1][1])(
                                list(
                                    filter(
                                        lambda x: x is not None,
                                        map(
                                            lambda i_char: next(
                                                filter(
                                                    lambda str_val: l[
                                                        i_char[0] :
                                                    ].startswith(str_val[0]),
                                                    [
                                                        ("1", 1),
                                                        ("2", 2),
                                                        ("3", 3),
                                                        ("4", 4),
                                                        ("5", 5),
                                                        ("6", 6),
                                                        ("7", 7),
                                                        ("8", 8),
                                                        ("9", 9),
                                                        ("one", 1),
                                                        ("two", 2),
                                                        ("three", 3),
                                                        ("four", 4),
                                                        ("five", 5),
                                                        ("six", 6),
                                                        ("seven", 7),
                                                        ("eight", 8),
                                                        ("nine", 9),
                                                    ],
                                                ),
                                                None,
                                            ),
                                            enumerate(l),
                                        ),
                                    )
                                )
                            ),
                            input,
                        )
                    )
                ),
            ]
        )(open("../input.txt").read().strip().splitlines())
    )
)
