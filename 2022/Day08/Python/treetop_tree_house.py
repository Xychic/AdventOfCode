print(
    "\n".join(
        (lambda input, row_len, col_len: [
            "Part 1: {}".format(
                sum(
                    map(
                        lambda row: len(
                            list(
                                filter(
                                    lambda tree_pair: (
                                        lambda x, y, tree: all(
                                            map(
                                                lambda x2: input[y][x2][1] < tree,
                                                range(x)
                                            )
                                        ) or all(
                                            map(
                                                lambda x2: input[y][x2][1] < tree,
                                                range(x+1, row_len)
                                            )
                                        ) or all(
                                            map(
                                                lambda y2: input[y2][x][1] < tree,
                                                range(y)
                                            )
                                        ) or all(
                                            map(
                                                lambda y2: input[y2][x][1] < tree,
                                                range(y+1, col_len)
                                            )
                                        )
                                    )(
                                        tree_pair[0][0], tree_pair[0][1], tree_pair[1]
                                    ),
                                    row
                                )
                            )
                        ),
                        input
                    )
                )
            ),
            "Part 2: {}".format(
                max(
                    map(
                        lambda row: max(
                            map(
                                lambda tree_pair: (
                                    lambda x, y, tree, takewhile, corrector: corrector(
                                        len(
                                            list(
                                                takewhile(
                                                    lambda y2: input[y2][x][1] < tree,
                                                    range(y-1, -1, -1)
                                                )
                                            )
                                        ),
                                        y
                                    ) * corrector(
                                        len(
                                            list(
                                                takewhile(
                                                    lambda x2: input[y][x2][1] < tree, 
                                                    range(x-1, -1, -1)
                                                )
                                            )
                                        ),
                                        x
                                    ) * corrector(
                                        len(
                                            list(
                                                takewhile(
                                                    lambda x2: input[y][x2][1] < tree,
                                                    range(x+1, len(input[0]))
                                                )
                                            )
                                        ),
                                        row_len - x - 1
                                    ) * corrector(
                                        len(
                                            list(
                                                takewhile(
                                                    lambda y2: input[y2][x][1] < tree,
                                                    range(y+1, len(input))
                                                )
                                            )
                                        ),
                                        col_len - y - 1
                                    )
                                )(
                                    tree_pair[0][0],
                                    tree_pair[0][1],
                                    tree_pair[1],
                                    __import__("itertools").takewhile,
                                    (lambda val, len: val+1 if val < len else val)
                                ),
                                row
                            )
                        ),
                        input
                    )
                )
            )
        ])(
            *(
                lambda input: (input, len(input[0]), len(input))
            )(
                [
                    [
                        (
                            (x, y),
                            int(tree)
                        )
                        for (x, tree) in enumerate(line)
                    ]
                    for (y, line) in enumerate(
                        open("../input.txt").read().strip().splitlines()
                    )
                ]
            )
        )
    )
)
