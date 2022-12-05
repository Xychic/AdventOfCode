print(
    "\n".join(
        (
            lambda towers, moves: [
                "Part 1: {}".format(
                    (
                        lambda towers: "".join(map(lambda tower: tower[-1], filter(lambda tower: len(tower) != 0, towers)))
                    )(
                        (
                            lambda towers, moves: [
                                [[towers[end].append(towers[start].pop()) for _ in range(count)] for (count, start, end) in moves],
                                towers
                            ][1]
                        )([tower[::] for tower in towers], moves)
                    )
                ),"Part 2: {}".format(
                    (
                        lambda towers: "".join(map(lambda tower: tower[-1], filter(lambda tower: len(tower) != 0, towers)))
                    )(
                        (
                            lambda towers, moves: [
                                [towers[end].extend([towers[start].pop() for _ in range(count)][::-1]) for (count, start, end) in moves],
                                towers
                            ][1]
                        )(towers, moves)
                    )
                )
            ]
        )(
            *(
                lambda towers, moves: (
                    (
                        lambda tower_lines: (
                            lambda lines, towers: [
                                [
                                    towers[(index+1) // 4].append(c) for line in lines for (index, c) in filter(lambda x: x[1].isalpha(), enumerate(line))], 
                                    towers
                                ][1]
                        )(tower_lines[:-1][::-1], [[] for _ in range((len(tower_lines[-1])+1)// 4)])
                    )(towers.splitlines()),
                    list(
                        map(
                            lambda line: (
                                lambda parts: (int(parts[1]), int(parts[3])-1, int(parts[5])-1)
                            )(line.split()),
                            moves.splitlines()
                        )
                    )
                )
            )(*open("../input.txt").read().split("\n\n"))
        )
    )
)
