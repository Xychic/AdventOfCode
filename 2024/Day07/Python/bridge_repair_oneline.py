# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda x: x[0],
                            filter(
                                lambda x: (
                                    lambda r1: lambda t, n: r1(
                                        r1,
                                        t,
                                        n
                                    )
                                )(
                                    lambda r1, t, n: n[0] == t if len(n) == 1 else (
                                        t % n[-1] == 0 and r1(
                                            r1,
                                            t//n[-1],
                                            n[:-1]
                                        )
                                    ) or (
                                        t > n[-1] and r1(
                                            r1,
                                            t-n[-1],
                                            n[:-1]
                                        )
                                    )
                                )(
                                    x[0],x[1]
                                ),
                                input
                            )
                        )
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda x: x[0],
                            filter(
                                lambda x: (
                                    lambda r2: lambda t, n: r2(
                                        r2,
                                        t,
                                        n
                                    )
                                )(
                                    lambda r2, t, n: n[0] == t if len(n) == 1 else (
                                        t % n[-1] == 0 and r2(
                                            r2,
                                            t//n[-1],
                                            n[:-1]
                                        )
                                    ) or (
                                        t > n[-1] and r2(
                                            r2,
                                            t-n[-1],
                                            n[:-1]
                                        )
                                    ) or (
                                        str(t).endswith(
                                            str(n[-1])
                                        ) and r2(
                                            r2,
                                            (t-n[-1]) // (
                                                10**len(
                                                    str(n[-1])
                                                )
                                            ),
                                            n[:-1]
                                        )
                                    )
                                )(
                                    x[0],
                                    x[1]
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
                    lambda l: (
                        lambda a, b: (
                            int(a),
                            list(
                                map(
                                    int,
                                    b.split(" ")
                                )
                            )
                        )
                    )(
                        *l.split(": ")
                    ),
                    open("../input.txt").read().strip().splitlines()
                )
            )
        )
    )
)
