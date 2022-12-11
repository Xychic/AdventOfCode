print(
    "\n".join(
        (
            lambda input, part_1, part_2: [
                "Part 1: {}".format(
                    part_1([{k: v if k != "items" else v[:] for k, v in x.items()} for x in input])
                ), "Part 2: {}".format(
                    part_2(input)
                )
            ]
        )(
            list(
                map(
                    lambda monkey_str: (
                        lambda id, items, op, test, true, false: {
                            "id": int(id[:-1].split(" ")[1]),
                            "items": [int(x) for x in items.split(": ")[1].split(", ")],
                            "op": lambda old: eval(op.split("= ")[-1]),
                            "test_val":int(test.split()[-1]),
                            "test": lambda val: [
                                int(false.split()[-1]), 
                                int(true.split()[-1])
                            ][val == 0],
                            "inspected": 0
                        }
                    )(
                        *monkey_str.splitlines()
                    ),
                    open("../input.txt").read().split("\n\n")
                )
            ),
            (
                lambda input: (
                    [
                        [
                            [
                                (
                                    lambda item: input[m["test"](item % m["test_val"])]["items"].append(item)
                                ) (
                                    (
                                        m.update({"inspected": m["inspected"] + 1}),
                                        m["op"](m["items"].pop()) // 3
                                    )[1]
                                )
                                for _ in range(len(m["items"]))
                            ]
                            for m in input
                        ]
                        for _ in range(20)
                    ],
                    (
                        lambda a, b: a * b
                    )(
                        *sorted([m["inspected"] for m in input], reverse=True)[:2]
                    )
                )[1]
            ),
            (
                lambda input: (
                    (
                        lambda input, modulo: (
                            [
                                [
                                    [
                                        (
                                            lambda item: input[m["test"](item % m["test_val"])]["items"].append(item)
                                        ) (
                                            (
                                                m.update({"inspected": m["inspected"] + 1}),
                                                m["op"](m["items"].pop()) % modulo
                                            )[1]
                                        )
                                        for _ in range(len(m["items"]))
                                    ]
                                    for m in input
                                ]
                                for _ in range(10_000)
                            ],
                            (
                                lambda a, b: a * b
                            )(
                                *sorted([m["inspected"] for m in input], reverse=True)[:2]
                            )
                        )[1]
                    )(
                        input,
                        (
                            lambda input: (
                                lambda xs: (
                                    lambda data, xs: (
                                        [
                                            data.update({"acc": data["acc"] * x})
                                            for x in xs
                                        ],
                                        data["acc"]
                                    )[1]
                                )(
                                    {"acc": xs[0]}, 
                                    xs[1:]
                                )
                            )(
                                [m["test_val"] for m in input]
                            )
                        )(input)
                    )
                )
            )
        )
    )
)