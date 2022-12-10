print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    (
                        lambda input, data: int(
                            [
                                [
                                    [
                                        data.update(
                                            {
                                                "clock": data["clock"] + 1
                                            }
                                        ),
                                        data.update(
                                            {
                                                "ans": data["ans"] + data["clock"] * data["x"]
                                            }
                                        ) if data["clock"] % 40 == 20 else (),
                                    ] if ins == "noop" else [
                                        [
                                            [
                                                data.update(
                                                    {
                                                        "clock": data["clock"] + 1
                                                    }
                                                ),
                                                data.update(
                                                    {
                                                        "ans": data["ans"] + data["clock"] * data["x"]
                                                    }
                                                ) if data["clock"] % 40 == 20 else ()
                                            ] for _ in range(2)
                                        ],
                                        data.update(
                                            {
                                                "x": data["x"] + float(val)
                                            }
                                        )
                                    ]
                                    for (ins, val) in input
                                ],
                                data["ans"]
                            ][1]
                        )
                    )(
                        input, {
                            "x": 1,
                            "clock": 0,
                            "ans": 0
                        }
                    )
                ), "Part 2: \n{}".format(
                    (
                        lambda input, data: "\n".join(
                            map(
                                lambda row: "".join(row.values()),
                                [
                                    [
                                        [
                                            data["screen"][data["clock"] // 40].update(
                                                {
                                                    data["clock"] % 40: "█" if abs(data["x"] - (data["clock"] % 40)) <= 1 else "░"
                                                }
                                            ),
                                            data.update(
                                                {
                                                    "clock": data["clock"] + 1
                                                }
                                            )
                                        ] if ins == "noop" else [
                                            [
                                                [
                                                    data["screen"][data["clock"] // 40].update(
                                                        {
                                                            data["clock"] % 40: "█" if abs(data["x"] - (data["clock"] % 40)) <= 1 else "░"
                                                        }
                                                    ),
                                                    data.update(
                                                        {
                                                            "clock": data["clock"] + 1
                                                        }
                                                    )
                                                ] for _ in range(2)
                                            ],
                                            data.update(
                                                {
                                                    "x": data["x"] + float(val)
                                                }
                                            )
                                        ]
                                        for (ins, val) in input
                                    ],
                                    data["screen"]
                                ][1].values()
                            )
                        )
                    ) (
                        input, {
                            "x": 1,
                            "clock": 0,
                            "screen": {
                                y: {
                                    x: "" for x in range(40)
                                } for y in range(6)
                            }
                        }
                    )
                )
            ]
        )(
            list(
                map(
                    lambda line: (line, 0) if line[0] == "n" else tuple(line.split()),
                    open("../input.txt").read().strip().splitlines()
                )
            )
        )
    )
)