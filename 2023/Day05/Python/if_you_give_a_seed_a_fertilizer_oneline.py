# type: ignore

print(
    "\n".join(
        (
            lambda seeds, almanac: [
                "Part 1: {}".format(
                    min(
                        map(
                            lambda s: (lambda r: lambda iter, acc: r(r, iter, acc))(
                                lambda r, iter, acc: acc
                                if not iter
                                else r(
                                    r,
                                    iter[1:],
                                    (lambda r: lambda item, map: r(r, item, map))(
                                        lambda r, item, map: item
                                        if not map
                                        else item + map[0][0] - map[0][1]
                                        if map[0][1] <= item <= map[0][1] + map[0][2]
                                        else r(r, item, map[1:])
                                    )(acc, iter[0]),
                                )
                            )(almanac, s),
                            seeds,
                        )
                    )
                ),
                "Part 2: {}".format(
                    min(
                        (lambda r: lambda iter, acc: r(r, iter, acc))(
                            lambda r, iter, acc: acc
                            if not iter
                            else r(
                                r,
                                iter[1:],
                                (lambda r: lambda map, q, result: r(r, map, q, result))(
                                    lambda r, map, q, result: result + q
                                    if not map
                                    else r(
                                        r,
                                        map[1:],
                                        *(
                                            lambda r: lambda q, dest, start, end, result, new_q: r(
                                                r, q, dest, start, end, result, new_q
                                            )
                                        )(
                                            lambda r, q, dest, start, end, result, new_q: (
                                                new_q,
                                                result,
                                            )
                                            if not q
                                            else r(
                                                r,
                                                q[1:],
                                                dest,
                                                start,
                                                end,
                                                result
                                                + (
                                                    [
                                                        (
                                                            max(q[0][0], start)
                                                            + dest
                                                            - start,
                                                            min(q[0][1], end)
                                                            + dest
                                                            - start,
                                                        )
                                                    ]
                                                    if max(q[0][0], start)
                                                    < min(q[0][1], end)
                                                    else []
                                                ),
                                                new_q
                                                + (
                                                    [(q[0][0], min(q[0][1], start))]
                                                    if q[0][0] < min(q[0][1], start)
                                                    else []
                                                )
                                                + (
                                                    [(max(q[0][0], end), q[0][1])]
                                                    if max(q[0][0], end) < q[0][1]
                                                    else []
                                                ),
                                            )
                                        )(
                                            q,
                                            map[0][0],
                                            map[0][1],
                                            map[0][1] + map[0][2],
                                            result,
                                            [],
                                        )
                                    )
                                )(iter[0], acc, []),
                            )
                        )(
                            almanac,
                            [(a, a + b) for a, b in zip(seeds[::2], seeds[1::2])],
                        )
                    )[0]
                ),
            ]
        )(
            *(
                lambda lines: (
                    list(map(int, (lines[0].split(": ")[1]).split())),
                    list(
                        map(
                            lambda x: list(
                                map(
                                    lambda a: tuple(map(int, a.split())),
                                    x.splitlines()[1:],
                                )
                            ),
                            lines[1:],
                        )
                    ),
                )
            )(open("../input.txt").read().strip().split("\n\n"))
        )
    )
)
