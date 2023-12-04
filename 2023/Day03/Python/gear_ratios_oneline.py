# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(sum(map(lambda x: x[0], input))),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda c: sum(
                                (lambda r: lambda iter, n, res: r(r, iter, n, res))(
                                    lambda r, iter, n, res: res
                                    if not iter
                                    else r(r, iter[1:], iter[0], res)
                                    if n is None
                                    else r(r, iter[1:], None, res + [iter[0][0] * n[0]])
                                    if iter[0][1] == n[1]
                                    else r(r, iter[1:], iter[0], res)
                                )(c, None, [])
                            ),
                            (lambda r: lambda iter, res: r(r, iter, res))(
                                lambda r, iter, res: res
                                if not iter
                                else r(r, iter[99:], res + [iter[:100]])
                            )(sorted(input, key=lambda x: list(x[1])[0]), []),
                        )
                    )
                ),
            ]
        )(
            (lambda r: lambda iter, res: r(r, iter, res))(
                lambda r, iter, res: res
                if not iter
                else r(r, iter[1:], res + list(iter[0]))
            )(
                list(
                    (
                        lambda input: map(
                            lambda line: filter(
                                lambda num_adjacent: any(
                                    map(
                                        len,
                                        num_adjacent[1],
                                    )
                                ),
                                (
                                    lambda r: lambda iter, num, adjacent, nums: r(
                                        r, iter, num, adjacent, nums
                                    )
                                )(
                                    lambda r, iter, num, adjacent, nums: nums
                                    if not iter and num == 0
                                    else nums + [(num, adjacent)]
                                    if not iter
                                    else r(
                                        r,
                                        iter[1:],
                                        num * 10 + int(iter[0][1]),
                                        adjacent.union(
                                            {
                                                (iter[0][0] + dx, line[0] + dy)
                                                for (dx, dy) in [
                                                    (-1, -1),
                                                    (0, -1),
                                                    (1, -1),
                                                    (-1, 0),
                                                    (1, 0),
                                                    (-1, 1),
                                                    (0, 1),
                                                    (1, 1),
                                                ]
                                                if 0 <= (iter[0][0] + dx) < len(line[1])
                                                and 0 <= (line[0] + dy) < len(input)
                                                and input[line[0] + dy][1][
                                                    iter[0][0] + dx
                                                ]
                                                not in [
                                                    ".",
                                                    "0",
                                                    "1",
                                                    "2",
                                                    "3",
                                                    "4",
                                                    "5",
                                                    "6",
                                                    "7",
                                                    "8",
                                                    "9",
                                                ]
                                            }
                                        ),
                                        nums,
                                    )
                                    if iter[0][1].isdigit()
                                    else r(
                                        r,
                                        iter[1:],
                                        0,
                                        set(),
                                        nums + [(num, adjacent)],
                                    )
                                    if num != 0
                                    else r(r, iter[1:], 0, set(), nums)
                                )(
                                    list(enumerate(line[1])), 0, set(), []
                                ),
                            ),
                            input,
                        )
                    )(
                        list(
                            enumerate(
                                list(open("../input.txt").read().strip().splitlines())
                            )
                        )
                    )
                ),
                [],
            )
        )
    )
)
