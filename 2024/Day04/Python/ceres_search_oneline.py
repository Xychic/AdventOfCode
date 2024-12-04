# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda y: sum(
                                map(
                                    lambda x: 0 if input[y][x] != 'X' else sum(
                                        map(
                                            lambda d: all(
                                                map(
                                                    lambda i:
                                                        input[y + i*d[1]][x+i*d[0]] == "XMAS"[i],
                                                        range(4)
                                                )
                                            ) if 0 <= (x + 3*d[0]) < len(input[y]) and 0 <= (y + 3*d[1]) < len(input) else 0,
                                            [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]
                                        )
                                    ),
                                    range(len(input[y]))
                                )
                            ),
                            range(len(input))
                        )
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda y: sum(
                                map(
                                    lambda x: 0 if input[y][x] != "A" else sum(
                                        map(
                                            lambda ds: all(
                                                map(
                                                    lambda i: input[y+ds[i][1]][x+ds[i][0]] == "MSMS"[i],
                                                    range(4)
                                                )
                                            ),
                                            [
                                                [(-1, -1),(1, 1),(1, -1),(-1, 1)],
                                                [(-1, -1),(1, 1),(-1, 1),(1, -1)],
                                                [(1, 1),(-1, -1),(1, -1),(-1, 1)],
                                                [(1, 1),(-1, -1),(-1, 1),(1, -1)],
                                            ]
                                        )
                                    ),
                                    range(1, len(input[y])-1)
                                )
                            ),
                            range(1, len(input)-1)
                        )
                    )
                )
            ]
        )(
            open("../input.txt").read().strip().splitlines()
        )
    )
)
