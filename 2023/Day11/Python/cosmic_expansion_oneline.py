# type: ignore

print(
    "\n".join(
        (
            lambda input, blank_cols, blank_rows: [
                "Part 1: {}".format(
                    (
                        lambda galaxies: sum(
                            map(
                                lambda y_g1: sum(
                                    map(
                                        lambda g2: abs(y_g1[1][0] - g2[0])
                                        + abs(y_g1[1][1] - g2[1]),
                                        galaxies[y_g1[0] :],
                                    )
                                ),
                                enumerate(galaxies),
                            )
                        )
                    )(
                        sum(
                            list(
                                map(
                                    lambda y_line: list(
                                        map(
                                            lambda g: g[0],
                                            filter(
                                                lambda g: g[1] == "#",
                                                map(
                                                    lambda x_char: (
                                                        (
                                                            x_char[0]
                                                            + len(
                                                                list(
                                                                    filter(
                                                                        lambda col: col
                                                                        < x_char[0],
                                                                        blank_cols,
                                                                    )
                                                                )
                                                            ),
                                                            y_line[0]
                                                            + len(
                                                                list(
                                                                    filter(
                                                                        lambda row: row
                                                                        < y_line[0],
                                                                        blank_rows,
                                                                    )
                                                                )
                                                            ),
                                                        ),
                                                        x_char[1],
                                                    ),
                                                    enumerate(y_line[1]),
                                                ),
                                            ),
                                        )
                                    ),
                                    enumerate(input),
                                )
                            ),
                            [],
                        )
                    )
                ),
                "Part 2: {}".format((
                        lambda galaxies: sum(
                            map(
                                lambda y_g1: sum(
                                    map(
                                        lambda g2: abs(y_g1[1][0] - g2[0])
                                        + abs(y_g1[1][1] - g2[1]),
                                        galaxies[y_g1[0] :],
                                    )
                                ),
                                enumerate(galaxies),
                            )
                        )
                    )(
                        sum(
                            list(
                                map(
                                    lambda y_line: list(
                                        map(
                                            lambda g: g[0],
                                            filter(
                                                lambda g: g[1] == "#",
                                                map(
                                                    lambda x_char: (
                                                        (
                                                            x_char[0]
                                                            + 999_999 * len(
                                                                list(
                                                                    filter(
                                                                        lambda col: col
                                                                        < x_char[0],
                                                                        blank_cols,
                                                                    )
                                                                )
                                                            ),
                                                            y_line[0]
                                                            + 999_999 * len(
                                                                list(
                                                                    filter(
                                                                        lambda row: row
                                                                        < y_line[0],
                                                                        blank_rows,
                                                                    )
                                                                )
                                                            ),
                                                        ),
                                                        x_char[1],
                                                    ),
                                                    enumerate(y_line[1]),
                                                ),
                                            ),
                                        )
                                    ),
                                    enumerate(input),
                                )
                            ),
                            [],
                        )
                    )),
            ]
        )(
            *(
                lambda input: (
                    input,
                    list(
                        filter(
                            lambda x: all(map(lambda l: l[x] == ".", input)),
                            range(len(input[0])),
                        )
                    ),
                    list(
                        filter(
                            lambda y: all(map(lambda c: c == ".", input[y])),
                            range(len(input)),
                        )
                    ),
                )
            )(list(map(list, open("../input.txt").read().strip().splitlines())))
        )
    )
)
