print(
    "\n".join(
        (
            lambda input, priorities: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda line: priorities.index(
                                next(
                                    l for l in line[:len(line)//2] 
                                    if l in line[len(line)//2:]
                                )
                            ),
                            input
                        )
                    )
                ),"Part 2: {}".format(
                    sum(
                        map(
                            lambda chunk: priorities.index(
                                next(
                                    l for l in chunk[0]
                                    if l in chunk[1] and l in chunk[2]
                                )
                            ),
                            [input[i:i+3] for i in range(0, len(input), 3)]
                        )
                    )
                )
            ]
        )(
            list(
                map(
                    lambda line: "".join(line.split()),
                    open("../input.txt").readlines()
                )
            ),
            "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        )
    )
)