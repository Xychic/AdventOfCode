print(
    "\n".join(
        (lambda input: [
            "Part 1: {}".format(
                sum(
                    filter(
                        lambda x: x <= 100000,
                        input.values()
                    )
                )
            ),
            "Part 2: {}".format(
                min(
                    filter(
                        lambda x: x >= (30000000 - (70000000 - input["/"])),
                        input.values()
                    )
                )
            )
        ])(
            (
                lambda lines, data, path: (
                    (
                        [
                            (
                                path.pop() if line.startswith("$ cd ..") else
                                (
                                    path.append(line[5:]) if line.startswith("$ cd") else
                                    (
                                        "DO NOTHING" if (line.startswith("dir") or line.startswith("$ ls")) else
                                        [
                                            (
                                                lambda to_update:
                                                data.update(
                                                    {
                                                        to_update: int(line.split()[0]) + data.get(to_update, 0)
                                                    }
                                                )
                                            )(
                                                "/".join(path[:i+1])
                                            )
                                            for i in range(len(path))
                                        ]
                                    )
                                )
                            )
                            for line in lines
                        ],
                        data,
                    )[1]
                )
            )(
                open("../input.txt").read().strip().splitlines(),
                {},
                []
            )
        )
    )
)
