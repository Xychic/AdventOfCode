print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda line: (lambda a0, a1, b0, b1: (a0 <= b0 and b1 <= a1) or (b0 <= a0 and a1 <= b1))(*line),
                            input
                        )
                    )
                ),"Part 2: {}".format(
                    sum(
                        map(
                            lambda line: (lambda a0, a1, b0, b1: max(a0, b0) <= min(a1, b1))(*line),
                            input
                        )
                    )
                )
            ]
        )(
            list(
                map(
                    lambda line: list(
                        map(
                            int,
                            line.replace(",","-").split("-")
                        )
                    ),
                    open("../input.txt").read().splitlines()
                )
            )
        )
    )
)
