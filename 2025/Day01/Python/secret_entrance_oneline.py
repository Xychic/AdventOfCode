# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    (lambda r1: lambda input, pos, count: r1(r1, input, pos, count))(
                        lambda r1, input, pos, count: 
                        count if input == [] 
                        else r1(
                            r1, 
                            *(lambda r2: lambda input, pos, count, depth: r2(r2, input, pos, count, depth))(
                                lambda r2, input, pos, count, depth: 
                                    (input, pos, count) if input == [] or depth == 0 
                                    else r2(
                                        r2, 
                                        input[1:],
                                        ((pos + input[0]) % 100), 
                                        count + (((pos + input[0]) % 100) == 0), depth-1
                                    )
                            )(
                                input, 
                                pos, 
                                count, 
                                500
                            )
                        )
                    )(input, 50, 0)
                ),
                "Part 2: {}".format(
                    (
                        lambda r6: lambda input, pos, count: r6(r6, input, pos, count)
                    )(
                        lambda r6, input, pos, count:
                        count if input == []
                        else r6(
                            r6,
                            *(
                                lambda r5: lambda input, pos, count, depth: r5(r5, input, pos, count, depth)
                            )(
                                lambda r5, input, pos, count, depth:
                                (input, pos, count) if input == [] or depth == 0
                                else r5(
                                    r5,
                                    input[1:], 
                                    *(
                                        lambda r4: lambda pos, step, reps, count: r4(r4, pos, step, reps, count)
                                    )(
                                        lambda r4, pos, step, reps, count:
                                        (pos, count) if reps == 0
                                        else r4(
                                            r4, 
                                            *(
                                                lambda r3: lambda pos, step, reps, count, depth: r3(r3, pos, step, reps, count, depth)
                                            )(
                                                lambda r3, pos, step, reps, count, depth:
                                                (pos, step, reps, count) if reps == 0 or depth == 0
                                                else r3(
                                                    r3, 
                                                    (pos + step) % 100, 
                                                    step, 
                                                    reps -1, 
                                                    count + ((pos + step) % 100 == 0), 
                                                    depth-1
                                                ) 
                                            )(
                                                pos,
                                                step,
                                                reps,
                                                count,
                                                100
                                            )
                                        )
                                    )(
                                        pos,
                                        input[0]/abs(input[0]), 
                                        abs(input[0]), 
                                        count
                                    ), 
                                    depth-1
                                )
                            )(
                                input,
                                pos, 
                                count, 
                                100
                            )
                        )
                    )(input, 50, 0)
                )
            ]
        )(
            list(
                map(
                    lambda l: (-1) ** (l[0] == 'L') * int(l[1:]), 
                    open("../input.txt").read().strip().splitlines()
                )
            )
        )
    )
)
