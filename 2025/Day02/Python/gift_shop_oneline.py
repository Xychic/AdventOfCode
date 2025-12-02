# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    (
                        lambda r1: lambda xs, acc: r1(r1, xs, acc)
                    )(
                        lambda r1, xs, acc: 
                        acc if xs == []
                        else r1(
                            r1,
                            xs[1:],
                            acc + sum(
                                filter(
                                    lambda x: len(str(x)) % 2 == 0 and x % ((10 ** (len(str(x)) // 2)) + 1) == 0,
                                    range(xs[0][0], xs[0][1]+1)
                                )
                            )
                        )
                    )(input, 0)
                ),
                "Part 2: {}".format(
                    (
                        lambda r1: lambda xs, acc: r1(r1, xs, acc)
                    )(
                        lambda r1, xs, acc: 
                        acc if xs == []
                        else r1(
                            r1,
                            xs[1:],
                            acc + sum(
                                filter(
                                    lambda x: {
                                        2: x % 11 == 0,
                                        3: x % 111 == 0,
                                        4: x % 101 == 0,
                                        5: x % 11111 == 0,
                                        6: x % 10101 == 0 or x % 1001 == 0,
                                        7: x % 1111111 == 0,
                                        8: x % 1010101 == 0 or x % 10001 == 0,
                                        9: x % 1001001 == 0,
                                        10: x % 101010101 == 0 or x % 100001 == 0,
                                    }.get(len(str(x)), False),
                                    range(xs[0][0], xs[0][1]+1)
                                )
                            )
                        )
                    )(input, 0)
                )
            ]
        )(
            list(
                map(
                    lambda l: tuple(
                        map(
                            int,
                            l.split("-")
                        )
                    ),
                    open("../input.txt").read().strip().split(",")
                )
            )
        )
    )
)
