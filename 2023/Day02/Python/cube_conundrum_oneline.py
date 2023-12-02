# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda g: g[0]
                            if all(
                                map(
                                    lambda c: int(c[0])
                                    <= {"red": 12, "green": 13, "blue": 14}[c[1]],
                                    g[1],
                                )
                            )
                            else 0,
                            input,
                        )
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda l: (lambda r: lambda i, s: r(r, i, s))(
                                lambda r, i, s: s if not i else r(r, i[1:], s * i[0])
                            )(
                                (lambda r: lambda i, acc: r(r, i, acc))(
                                    lambda r, i, acc: acc
                                    if not i
                                    else r(
                                        r,
                                        i[1:],
                                        {
                                            "red": (
                                                max(int(i[0][0]), acc[0]),
                                                acc[1],
                                                acc[2],
                                            ),
                                            "green": (
                                                acc[0],
                                                max(int(i[0][0]), acc[1]),
                                                acc[2],
                                            ),
                                            "blue": (
                                                acc[0],
                                                acc[1],
                                                max(int(i[0][0]), acc[2]),
                                            ),
                                        }[i[0][1]],
                                    )
                                )(
                                    l[1],
                                    (1, 1, 1),
                                ),
                                1,
                            ),
                            input,
                        )
                    )
                ),
            ]
        )(
            list(
                map(
                    lambda l: (
                        lambda x, y: (
                            int(x.split()[1]),
                            list(map(str.split, y.split(", "))),
                        )
                    )(*(l.split(": ", 1))),
                    open("../input.txt").read().strip().replace(";", ",").splitlines(),
                )
            )
        )
    )
)
