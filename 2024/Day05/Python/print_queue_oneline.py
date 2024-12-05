# type: ignore
print(
    "\n".join(
        (
            lambda rules, updates: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda xs: xs[len(xs) // 2],
                            filter(
                                lambda xs: all(
                                    map(
                                        lambda a, b: b in rules.get(a, set()),
                                        xs,
                                        xs[1:]
                                    )
                                ),
                                updates
                            )
                        )
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda xss: xss[1][len(xss[1]) // 2],
                            filter(
                                lambda xss: xss[0] != xss[1],
                                map(
                                    lambda xs: (xs, (
                                        lambda r4: lambda xs, c: r4(r4, xs, c)
                                    )(
                                        lambda r4, xs, c: xs if c == 0 else r4(r4, *(lambda r5: lambda xs: r5(r5, xs))(lambda r5, xs: (xs, 0) if len(xs) == 1 else tuple(
                                            map(
                                                lambda a, b: a + b,
                                                ([xs[1]], 1),
                                                r5(r5, [xs[0]] + xs[2:])
                                            )
                                        ) if xs[0] in rules.get(xs[1], set()) else tuple(
                                            map(
                                                lambda a, b: a + b,
                                                ([xs[0]], 0),
                                                r5(r5, xs[1:])
                                            )
                                        ))(xs))
                                    )(xs, 1)),
                                    updates
                                )
                            )
                        )
                    )
                )
            ]
        )(
            *(lambda a, b: ((
                    lambda r1: lambda xs: r1(r1, xs)
                )(
                    lambda r1, xs: dict() if xs == [] else {xs[0][0]: (
                        lambda r2: lambda xs, val: r2(r2, xs, val)
                    )(
                        lambda r2, xs, val: set() if xs == [] or xs[0][0] != val else ({xs[0][1]} | r2(r2, xs[1:], val))
                    )(xs, xs[0][0])} | r1(r1, (
                        lambda r3: lambda xs, val: r3(r3, xs, val)
                    )(lambda r3, xs, val: [] if xs == [] else r3(r3, xs[1:], val) if xs[0][0] == val else xs)(xs, xs[0][0]))
                )(
                    sorted(
                        map(
                            lambda l: tuple(map(int, l.split('|'))),
                            a.splitlines()
                        ),
                        key=lambda x: x[0]
                    )
                ), list(
                    map(
                        lambda l: list(
                            map(
                                int,
                                l.split(',')
                            )
                        ),
                        b.splitlines()
                    )
                )
            ))(*open("../input.txt").read().strip().split("\n\n"))
        )
    )
)
