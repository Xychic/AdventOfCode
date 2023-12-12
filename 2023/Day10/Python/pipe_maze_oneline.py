# type: ignore

print(
    "\n".join(
        (
            lambda pipe_map, loop_set: [
                "Part 1: {}".format(len(loop_set) // 2),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda y_line: sum(
                                map(
                                    lambda x: len(
                                        list(
                                            filter(
                                                lambda i: y_line[1][i]
                                                in ["|", "7", "F"]
                                                and (i, y_line[0]) in loop_set,
                                                range(x),
                                            )
                                        )
                                    )
                                    % 2
                                    == 1,
                                    filter(
                                        lambda x: (x, y_line[0]) not in loop_set,
                                        range(len(y_line[1])),
                                    ),
                                )
                            ),
                            enumerate(pipe_map),
                        )
                    )
                ),
            ]
        )(
            *(
                lambda pipe_map: (
                    lambda start, char: (
                        lambda x, y, pipe_map: (
                            pipe_map,
                            (
                                lambda r: lambda pipe_map, start, x, y, dir, in_loop: r(
                                    r, pipe_map, start, x, y, dir, in_loop
                                )
                            )(
                                lambda r, pipe_map, start, x, y, dir, in_loop: set(
                                    in_loop
                                )
                                if (x, y) == start and in_loop
                                else r(
                                    r,
                                    pipe_map,
                                    start,
                                    *(
                                        (
                                            lambda r: lambda pipe_map, start, x, y, dir, in_loop, steps: r(
                                                r,
                                                pipe_map,
                                                start,
                                                x,
                                                y,
                                                dir,
                                                in_loop,
                                                steps,
                                            )
                                        )(
                                            lambda r, pipe_map, start, x, y, dir, in_loop, steps: (
                                                x,
                                                y,
                                                dir,
                                                in_loop,
                                            )
                                            if steps == 900
                                            else (x, y, dir, in_loop)
                                            if (x, y) == start and in_loop
                                            else r(
                                                r,
                                                pipe_map,
                                                start,
                                                [x, x + 1, x, x - 1][dir],
                                                [y - 1, y, y + 1, y][dir],
                                                dir
                                                if pipe_map[[y - 1, y, y + 1, y][dir]][
                                                    [x, x + 1, x, x - 1][dir]
                                                ]
                                                in ["|", "-"]
                                                else 1
                                                if (
                                                    dir == 2
                                                    and pipe_map[
                                                        [y - 1, y, y + 1, y][dir]
                                                    ][[x, x + 1, x, x - 1][dir]]
                                                    == "L"
                                                )
                                                or (
                                                    dir == 0
                                                    and pipe_map[
                                                        [y - 1, y, y + 1, y][dir]
                                                    ][[x, x + 1, x, x - 1][dir]]
                                                    == "F"
                                                )
                                                else 0
                                                if (
                                                    dir == 3
                                                    and pipe_map[
                                                        [y - 1, y, y + 1, y][dir]
                                                    ][[x, x + 1, x, x - 1][dir]]
                                                    == "L"
                                                )
                                                or (
                                                    dir == 1
                                                    and pipe_map[
                                                        [y - 1, y, y + 1, y][dir]
                                                    ][[x, x + 1, x, x - 1][dir]]
                                                    == "J"
                                                )
                                                else 3
                                                if (
                                                    dir == 2
                                                    and pipe_map[
                                                        [y - 1, y, y + 1, y][dir]
                                                    ][[x, x + 1, x, x - 1][dir]]
                                                    == "J"
                                                )
                                                or (
                                                    dir == 0
                                                    and pipe_map[
                                                        [y - 1, y, y + 1, y][dir]
                                                    ][[x, x + 1, x, x - 1][dir]]
                                                    == "7"
                                                )
                                                else 2,
                                                in_loop + [(x, y)],
                                                steps + 1,
                                            )
                                        )
                                    )(pipe_map, start, x, y, dir, in_loop, 0)
                                )
                            )(
                                pipe_map,
                                (x, y),
                                x,
                                y,
                                0
                                if char in ["J", "|", "L"]
                                else 1
                                if char in ["-", "F"]
                                else 2,
                                [],
                            ),
                        )
                    )(
                        start[0],
                        start[1],
                        (
                            pipe_map[: start[1]]
                            + [
                                pipe_map[start[1]][: start[0]]
                                + [char]
                                + pipe_map[start[1]][start[0] + 1 :]
                            ]
                            + pipe_map[start[1] + 1 :]
                        ),
                    )
                )(
                    *(
                        lambda pipe_map: (
                            lambda pos: (
                                pos,
                                (
                                    lambda u, d, l, r: "|"
                                    if (u and d and not l and not r)
                                    else "J"
                                    if (u and not d and l and not r)
                                    else "L"
                                    if (u and not d and not l and r)
                                    else "7"
                                    if (not u and d and l and not r)
                                    else "F"
                                    if (not u and d and not l and r)
                                    else "-"
                                )(
                                    pos[1] > 0
                                    and pipe_map[pos[1] - 1][pos[0]] in ["|", "F", "7"],
                                    pos[1] < len(pipe_map) - 1
                                    and pipe_map[pos[1] + 1][pos[0]]
                                    in [
                                        "|",
                                        "L",
                                        "J",
                                    ],
                                    pos[0] > 0
                                    and pipe_map[pos[1]][pos[0] - 1] in ["-", "L", "F"],
                                    pos[0] < len(pipe_map[pos[1]]) - 1
                                    and pipe_map[pos[1]][pos[0] + 1]
                                    in [
                                        "-",
                                        "J",
                                        "7",
                                    ],
                                ),
                            )
                        )(
                            next(
                                filter(
                                    bool,
                                    map(
                                        lambda y_line: next(
                                            filter(
                                                lambda pos_char: pos_char[1] == "S",
                                                map(
                                                    lambda x_char: (
                                                        (x_char[0], y_line[0]),
                                                        x_char[1],
                                                    ),
                                                    enumerate(y_line[1]),
                                                ),
                                            ),
                                            None,
                                        ),
                                        enumerate(pipe_map),
                                    ),
                                )
                            )[0]
                        )
                    )(pipe_map)
                )
            )(list(map(list, open("../input.txt").read().strip().splitlines())))
        )
    )
)
