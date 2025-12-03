# type: ignore
print(
    "\n".join(
        (
            lambda input: map(
                lambda x: x[0].format(x[1]),
                zip(
                    ["Part 1: {}", "Part 2: {}"],
                    map(
                        lambda x: sum(
                            map(
                                lambda xs: (
                                    lambda r1: lambda xs, size, seen: r1(r1, xs, size, seen)
                                )(
                                    lambda r1, xs, size, seen: (
                                        -2**16 if len(xs) < size else
                                        0 if size == 0 else
                                        seen.get((len(xs), size)) if (len(xs), size) in seen
                                        else [
                                            seen.update(
                                                {
                                                    (len(xs), size): max(
                                                        r1(r1, xs[1:], size, seen),
                                                        xs[0] * (10 ** (size-1)) + r1(r1, xs[1:], size-1, seen)
                                                    )
                                                }
                                            ),
                                            seen.get((len(xs), size))
                                        ][1]
                                    )
                                )(xs, x, {}),
                                input,
                            )
                        ),
                        [2, 12]
                    )
                )
            )
        )(
            list(
                map(
                    lambda l: list(
                        map(
                            int,
                            l
                        )
                    ),
                    open("../input.txt").read().strip().splitlines()
                )
            )
        )
    )
)
