# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda x: 0 if not type(x) == int else x,
                            input
                        )
                    )
                ),
                "Part 2: {}".format(
                    (lambda r3: lambda input: r3(r3, input))(
                        lambda r3, input: 
                        0 if not input else\
                        sum(
                            map(
                                lambda x: 0 if not type(x) == int else x,
                                (lambda r4: lambda input: r4(r4, input))(
                                    lambda r4, input: \
                                        [] if not input or input[0] == "don't()" \
                                        else [input[0]] + r4(r4, input[1:])
                                )(input)
                            )
                        ) + (lambda d: r3(
                                r3,
                                d(
                                    d(
                                        input,
                                        "don't()"
                                    ),
                                    "do()"
                                )
                            )
                        )(
                            (lambda r5: lambda input, target: r5(r5, input, target))(
                                lambda r5, input, target: \
                                    [] if not input else \
                                    r5(r5, input[1:], target) if input[0] != target \
                                    else input
                            )
                        )
                    )(input)
                )
            ]
        )(
            sum(
                filter(
                    bool,
                    map(
                        lambda x: (lambda r1: lambda string: r1(r1, string))(
                            lambda r1, string: [] if not string else \
                            ["do()"] + r1(r1,string[4:]) if string.startswith("do()") else \
                            ["don't()"] + r1(r1,string[7:]) if string.startswith("don't()") else \
                            [(lambda r2: lambda acc, xs:
                                r2(r2, acc, xs))(
                                    lambda r2, acc, xs: (
                                        lambda a: acc if a is None
                                        else r2(r2, acc * a, xs)
                                    )(
                                        next(xs, None)
                                    )
                                )(
                                    1,
                                    map(lambda x: 
                                        int(x) if x.isnumeric() else 0,
                                        string[4:12].split(")", 1)[0].split(',', 1)
                                    )
                                )
                            ] + r1(r1,string[4:]) if string.startswith("mul(") else \
                            r1(r1,string[1:])
                        )(x+")"),
                        # "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))".split(")")
                        open("../input.txt").read().strip().split(")")
                    )
                ),
                []
            )
        )
    )
)
