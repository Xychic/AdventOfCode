# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        (lambda r1: lambda galton, y, input: r1(r1, galton, y, input))(
                            lambda r1, galton, y, input: galton
                            if y == len(input) - 1
                            else r1(
                                r1,
                                list(
                                    map(
                                        lambda x: 0
                                        if input[y + 1][x] == "^"
                                        else galton[x]
                                        + (
                                            galton[x - 1]
                                            if x > 0 and input[y + 1][x - 1] == "^"
                                            else 0
                                        )
                                        + (
                                            (galton[x + 1] != 0)
                                            if x < len(input[y]) - 1
                                            and input[y + 1][x + 1] == "^"
                                            else 0
                                        ),
                                        range(len(input[y])),
                                    )
                                ),
                                y + 1,
                                input,
                            )
                        )([int(c == "S") for c in input[0]], 0, input)
                    )
                    - 1
                ),
                "Part 2: {}".format(
                    sum(
                        (lambda r1: lambda galton, y, input: r1(r1, galton, y, input))(
                            lambda r1, galton, y, input: galton
                            if y == len(input) - 1
                            else r1(
                                r1,
                                list(
                                    map(
                                        lambda x: 0
                                        if input[y + 1][x] == "^"
                                        else galton[x]
                                        + (
                                            galton[x - 1]
                                            if x > 0 and input[y + 1][x - 1] == "^"
                                            else 0
                                        )
                                        + (
                                            galton[x + 1]
                                            if x < len(input[y]) - 1
                                            and input[y + 1][x + 1] == "^"
                                            else 0
                                        ),
                                        range(len(input[y])),
                                    )
                                ),
                                y + 1,
                                input,
                            )
                        )([int(c == "S") for c in input[0]], 0, input)
                    )
                )
            ]
        )(
            list(
                map(
                    list,
                        open("../input.txt").read().strip().splitlines()
                )
            )
        )
    )
)
