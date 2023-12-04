# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda line: 0 if line[1] == 0 else 2 ** (line[1] - 1),
                            input,
                        )
                    )
                ),
                "Part 2: {}".format(
                    (lambda r: lambda iter, data: r(r, iter, data))(
                        lambda r, iter, data: sum(data)
                        if not iter
                        else r(
                            r,
                            iter[1:],
                            (
                                lambda r: lambda i, limit, x, data: r(
                                    r, i, limit, x, data
                                )
                            )(
                                lambda r, i, limit, x, data: data
                                if i > limit
                                else r(
                                    r,
                                    i + 1,
                                    limit,
                                    x,
                                    data[: x + i]
                                    + [data[x + i] + data[x]]
                                    + data[x + i + 1 :],
                                )
                            )(
                                1,
                                iter[0][1],
                                iter[0][0],
                                data[: iter[0][0]]
                                + [data[iter[0][0]] + 1]
                                + data[iter[0][0] + 1 :],
                            ),
                        )
                    )(input, [0 for _ in range(1 + len(input))])
                ),
            ]
        )(
            list(
                map(
                    lambda line: (
                        lambda x, y: (
                            int(x.split()[1]),
                            (
                                lambda a, b: (
                                    len(
                                        set(map(int, a.split()))
                                        & set(map(int, b.split()))
                                    )
                                )
                            )(*y.split(" | ")),
                        )
                    )(*line.split(": ")),
                    open("../input.txt").read().strip().splitlines(),
                )
            )
        )
    )
)
