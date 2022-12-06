print(
    "\n".join(
        (lambda input, solver: [
            "Part 1: {}".format(
                solver(input, 4)
            ),
            "Part 2: {}".format(
                solver(input, 14)
            )
        ])(
            open("../input.txt").readline().strip(),
            lambda input, size: next(
                filter(
                    lambda entry: entry[1] == size,
                    (
                        (
                            i+size,
                            len(
                                set(input[i:i+size])
                            )
                        )
                        for i in range(0, len(input)-size)
                    )
                )
            )[0]
        )
    )
)