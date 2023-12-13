# type: ignore

print(
    "\n".join(
        (
            lambda cycle, connections: [
                "Part 1: {}".format(
                    (
                        lambda r: lambda cycle, connections, current, i: r(
                            r, cycle, connections, current, i
                        )
                    )(
                        lambda r, cycle, connections, current, i: i
                        if current == "ZZZ"
                        else r(
                            r,
                            cycle,
                            connections,
                            *(
                                lambda r: lambda cycle, connections, current, i: r(
                                    r, cycle, connections, current, i
                                )
                            )(
                                lambda r, cycle, connections, current, i: (current, i)
                                if current == "ZZZ" or not cycle
                                else r(
                                    r,
                                    cycle[1:],
                                    connections,
                                    connections[current][cycle[0] == "R"],
                                    i + 1,
                                )
                            )(
                                cycle, connections, current, i
                            )
                        )
                    )(
                        cycle, connections, "AAA", 0
                    )
                ),
                "Part 2: {}".format(
                    (
                        (lambda r: lambda items: r(r, items))(
                            lambda r, items: items[0]
                            if len(items) == 1
                            else r(
                                r,
                                [
                                    (
                                        lambda a, b: a
                                        * b
                                        // (
                                            (lambda r: lambda a, b: r(r, a, b))(
                                                lambda r, a, b: abs(a)
                                                if b == 0
                                                else r(r, b, a % b)
                                            )
                                        )(a, b)
                                    )(items[0], items[1])
                                ]
                                + items[2:],
                            )
                        )
                    )(
                        list(
                            map(
                                lambda start: (
                                    (
                                        lambda r: lambda cycle, connections, current, i: r(
                                            r, cycle, connections, current, i
                                        )
                                    )(
                                        lambda r, cycle, connections, current, i: i
                                        if current.endswith("Z")
                                        else r(
                                            r,
                                            cycle,
                                            connections,
                                            *(
                                                (
                                                    lambda r: lambda cycle, connections, current, i: r(
                                                        r,
                                                        cycle,
                                                        connections,
                                                        current,
                                                        i,
                                                    )
                                                )(
                                                    lambda r, cycle, connections, current, i: (
                                                        current,
                                                        i,
                                                    )
                                                    if current.endswith("Z")
                                                    or not cycle
                                                    else r(
                                                        r,
                                                        cycle[1:],
                                                        connections,
                                                        connections[current][
                                                            cycle[0] == "R"
                                                        ],
                                                        i + 1,
                                                    )
                                                )
                                            )(cycle, connections, current, i)
                                        )
                                    )
                                )(cycle, connections, start, 0),
                                filter(lambda x: x.endswith("A"), connections.keys()),
                            )
                        )
                    )
                ),
            ]
        )(
            *(
                lambda cycle, map_str: (
                    cycle,
                    dict(
                        map(
                            lambda l: (l[0:3], (l[7:10], l[12:15])),
                            map_str.splitlines(),
                        )
                    ),
                )
            )(*open("../input.txt").read().strip().split("\n\n"))
        )
    )
)
