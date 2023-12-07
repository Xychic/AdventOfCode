# type: ignore
print(
    "\n".join(
        (
            lambda input, solve: [
                "Part 1: {}".format(solve(input, False)),
                "Part 2: {}".format(solve(input, True)),
            ]
        )(
            list(
                map(
                    lambda x: (
                        lambda a, b: (
                            list(
                                map(
                                    lambda c: {
                                        "T": 10,
                                        "J": 11,
                                        "Q": 12,
                                        "K": 13,
                                        "A": 14,
                                    }.get(c, ord(c) - ord("0")),
                                    a,
                                )
                            ),
                            int(b),
                        )
                    )(*x.split()),
                    open("../input.txt").read().strip().splitlines(),
                )
            ),
            lambda input, replace_jokers: sum(
                map(
                    lambda x: (x[0] + 1) * x[1][1],
                    enumerate(
                        sorted(
                            map(
                                lambda h: (
                                    lambda x, replace_jokers: (
                                        lambda x, replace_jokers, count: (
                                            lambda x, new_count, replace_jokers: (
                                                [
                                                    7
                                                    if new_count == [5]
                                                    else 6
                                                    if new_count == [1, 4]
                                                    else 5
                                                    if new_count == [2, 3]
                                                    else 4
                                                    if new_count == [1, 1, 3]
                                                    else 3
                                                    if new_count == [1, 2, 2]
                                                    else 2
                                                    if new_count == [1, 1, 1, 2]
                                                    else 1
                                                ]
                                                + list(
                                                    map(
                                                        lambda x: 0
                                                        if replace_jokers and x == 11
                                                        else x,
                                                        x[0],
                                                    )
                                                ),
                                                x[1],
                                            )
                                        )(
                                            x,
                                            sorted(
                                                filter(
                                                    bool,
                                                    (
                                                        lambda count, to_replace: (
                                                            count
                                                            if to_replace is None
                                                            else (
                                                                (
                                                                    count[:to_replace]
                                                                    + [
                                                                        count[
                                                                            to_replace
                                                                        ]
                                                                        + count[11]
                                                                    ]
                                                                    + count[
                                                                        to_replace
                                                                        + 1 : 11
                                                                    ]
                                                                    + [0]
                                                                    + count[12:]
                                                                )
                                                                if to_replace < 11
                                                                else (
                                                                    count[:11]
                                                                    + [0]
                                                                    + count[
                                                                        12:to_replace
                                                                    ]
                                                                    + [
                                                                        count[
                                                                            to_replace
                                                                        ]
                                                                        + count[11]
                                                                    ]
                                                                    + count[
                                                                        to_replace + 1 :
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )(
                                                        count,
                                                        next(
                                                            filter(
                                                                lambda x: x[0] != 11
                                                                and x[1] != 0,
                                                                sorted(
                                                                    enumerate(count),
                                                                    key=lambda x: x[1],
                                                                    reverse=True,
                                                                ),
                                                            ),
                                                            [None],
                                                        )[0],
                                                    )
                                                    if replace_jokers and count[11] != 0
                                                    else count,
                                                )
                                            ),
                                            replace_jokers,
                                        )
                                    )(
                                        x,
                                        replace_jokers,
                                        (lambda r: lambda acc, xs: r(r, acc, xs))(
                                            lambda r, acc, xs: acc
                                            if not xs
                                            else r(
                                                r,
                                                acc[: xs[0]]
                                                + [acc[xs[0]] + 1]
                                                + acc[xs[0] + 1 :],
                                                xs[1:],
                                            )
                                        )(list(map(lambda _: 0, range(15))), x[0]),
                                    )
                                )(h, replace_jokers),
                                input,
                            )
                        )
                    ),
                )
            ),
        )
    )
)
