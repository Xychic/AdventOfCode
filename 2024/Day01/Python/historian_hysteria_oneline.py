# type: ignore
print(
    "\n".join(
        (
            lambda l, r: [
                "Part 1: {}".format(
                    sum(
                        map(
                            lambda a,b: abs(a-b),
                            sorted(l),
                            sorted(r)
                        )
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda l: l * r.count(l),
                            l
                        )
                    )
                )
            ]
        )(
            *zip(
                *list(
                    map(
                        lambda line:
                            tuple(
                                map(
                                    int,
                                    line.split("   ")
                                )
                            ),
                        open("../input.txt").read().strip().splitlines()
                    )
                )
            )
        )
    )
)
