print(
    "\n".join(
        (lambda input, solve: [
            "Part 1: {}".format(
                solve(input, 2)
            ), "Part 2: {}".format(
                solve(input, 10)
            )
        ])(
            list(
                map(
                    lambda line: (
                        lambda dir, amount: ({
                            "U": (0, 1),
                            "D": (0, -1),
                            "L": (-1, 0),
                            "R": (1, 0)
                        }[dir], int(amount))
                    )(
                        *line.split()
                    ),
                    open("../input.txt").read().strip().splitlines()
                )
            ),
            lambda input, knot_count: (
                lambda input, knot_count, knots, seen, sign: [
                    [
                        [
                            [
                                knots.update(
                                    {
                                        0: (
                                            lambda a, b: tuple(
                                                map(
                                                    sum,
                                                    zip(a, b)
                                                )
                                            )
                                        )(
                                            knots[0], dir
                                        )
                                    }
                                ),
                                [
                                    knots.update(
                                        {
                                            i: (
                                                    knots[i][0] + sign(knots[i-1][0]-knots[i][0]),
                                                    knots[i][1] + sign(knots[i-1][1]-knots[i][1])
                                            ) if i != 0 and (abs(knots[i-1][0]-knots[i][0]) > 1 or abs(knots[i-1][1] - knots[i][1]) > 1) else knots[i]
                                        }
                                    )
                                    for i in range(knot_count)
                                ],
                                seen.add(knots[knot_count-1])
                            ]
                            for _ in range(amount)
                        ]
                        for dir, amount in input
                    ],
                    len(seen)
                ][1]
            )(
                input, knot_count, {i: (0,0) for i in range(knot_count)}, set(), lambda a: (a>0)-(a<0)
            )
        )
    )
)