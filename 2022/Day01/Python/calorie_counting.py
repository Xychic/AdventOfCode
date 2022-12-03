print(
    "\n".join(
        (lambda x: [
            f"Part 1: {max(x)}",
            f"Part 2: {sum(sorted(x,reverse=True)[:3])}"
        ])(
            list(
                map(
                    lambda elf: sum(
                        map(
                            int,
                            elf.splitlines()
                        )
                    ),
                    open("../input.txt").read().split("\n\n")
                )
            )
        )
    )
)