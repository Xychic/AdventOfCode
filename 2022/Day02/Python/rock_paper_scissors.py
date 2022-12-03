print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda line: "BXCYAZAXBYCZCXAYBZ".index(line) // 2 + 1,
                            input
                        )
                    )
                ),"Part 2: {}".format(
                    sum(
                        map(
                            lambda line: "BXCXAXAYBYCYCZAZBZ".index(line) // 2 + 1,
                            input
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
            )
        )
    )
)