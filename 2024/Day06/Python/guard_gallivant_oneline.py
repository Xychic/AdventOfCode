# type: ignore
print(
    "\n".join(
        (
            lambda input: [
                "Part 1: {}".format(
                    (
                        lambda r1: lambda dir, seen, guard, obstacles, gridsize: r1(
                            r1,
                            dir,
                            seen,
                            guard,
                            obstacles,
                            gridsize
                        )
                    )(
                        lambda r1, dir, seen, guard, obstacles, gridsize: r1(
                            r1,
                            *(
                                lambda r2: lambda dir, seen, guard, obstacles, gridsize, depth: r2(
                                    r2,
                                    dir,
                                    seen,
                                    guard,
                                    obstacles,
                                    gridsize,
                                    depth
                                )
                            )(
                                lambda r2, dir, seen, guard, obstacles, gridsize, depth: (
                                    dir,
                                    seen | {guard},
                                    guard
                                ) if not (0 <= guard[0] < gridsize[0] and 0 <= guard[1] < gridsize[1]) or depth > 500 else r2(
                                    r2,
                                    (dir+1)%4,
                                    seen,
                                    guard,
                                    obstacles,
                                    gridsize,
                                    depth+1
                                ) if [
                                    (guard[0], guard[1]-1),
                                    (guard[0]+1, guard[1]),
                                    (guard[0], guard[1]+1),
                                    (guard[0]-1, guard[1]),
                                ][dir] in obstacles else r2(
                                    r2,
                                    dir,
                                    seen | {guard},
                                    [
                                        (guard[0], guard[1]-1),
                                        (guard[0]+1, guard[1]),
                                        (guard[0], guard[1]+1),
                                        (guard[0]-1, guard[1]),
                                    ][dir],
                                    obstacles,
                                    gridsize,
                                    depth+1
                                )
                            )(
                                dir,
                                seen,
                                guard,
                                obstacles,
                                gridsize,
                                0
                            ),
                            obstacles,
                            gridsize
                        ) if (0 <= guard[0] < gridsize[0] and 0 <= guard[1] < gridsize[1]) else len(seen | {guard})
                    )(
                        0,
                        set(),
                        *input
                    )
                ),
                "Part 2: {}".format(
                    sum(
                        map(
                            lambda p: (
                                lambda r3: lambda dir, seen, looping, guard, obstacles, gridsize: r3(
                                    r3,
                                    dir,
                                    seen,
                                    looping,
                                    guard,
                                    obstacles,
                                    gridsize
                                )
                            )(
                                lambda r3, dir, seen, looping, guard, obstacles, gridsize: looping if looping or not (0 <= guard[0] < gridsize[0] and 0 <= guard[1] < gridsize[1]) else r3(
                                    r3,
                                    *(
                                        lambda r4: lambda dir, seen, looping, guard, obstacles, gridsize, depth: r4(
                                            r4,
                                            dir,
                                            seen,
                                            looping,
                                            guard,
                                            obstacles,
                                            gridsize,
                                            depth
                                        )
                                    )(
                                        lambda r4, dir, seen, looping, guard, obstacles, gridsize, depth: (
                                            dir,
                                            seen,
                                            looping,
                                            guard,
                                            obstacles,
                                            gridsize
                                        ) if depth > 800 or not (0 <= guard[0] < gridsize[0] and 0 <= guard[1] < gridsize[1]) else r4(
                                            r4,
                                            (dir+1)%4,
                                            seen,
                                            looping,
                                            guard,
                                            obstacles,
                                            gridsize,
                                            depth+1
                                        ) if [
                                            (guard[0], guard[1]-1),
                                            (guard[0]+1, guard[1]),
                                            (guard[0], guard[1]+1),
                                            (guard[0]-1, guard[1]),
                                        ][dir] in obstacles else (
                                            dir,
                                            seen,
                                            True,
                                            guard,
                                            obstacles,
                                            gridsize
                                        ) if [
                                            (guard[0], guard[1]-1, dir),
                                            (guard[0]+1, guard[1], dir),
                                            (guard[0], guard[1]+1, dir),
                                            (guard[0]-1, guard[1], dir),
                                        ][dir] in seen else r4(
                                            r4,
                                            dir,
                                            seen | {
                                                [
                                                    (guard[0], guard[1]-1, dir),
                                                    (guard[0]+1, guard[1], dir),
                                                    (guard[0], guard[1]+1, dir),
                                                    (guard[0]-1, guard[1], dir),
                                                ][dir]
                                            },
                                            looping,
                                            [
                                                (guard[0], guard[1]-1),
                                                (guard[0]+1, guard[1]),
                                                (guard[0], guard[1]+1),
                                                (guard[0]-1, guard[1]),
                                            ][dir],
                                            obstacles,
                                            gridsize,
                                            depth+1
                                        )
                                    )(
                                        dir,
                                        seen,
                                        looping,
                                        guard,
                                        obstacles,
                                        gridsize,
                                        0
                                    )
                                )
                            )(
                                0,
                                set(),
                                False,
                                input[0],
                                input[1] | {p},
                                input[2]
                            ),
                            (
                                lambda r1: lambda dir, seen, guard, obstacles, gridsize: r1(r1, dir, seen, guard, obstacles, gridsize)
                            )(
                                lambda r1, dir, seen, guard, obstacles, gridsize: r1(
                                    r1,
                                    *(
                                        lambda r2: lambda dir, seen, guard, obstacles, gridsize, depth: r2(r2, dir, seen, guard, obstacles, gridsize, depth)
                                    )(
                                        lambda r2, dir, seen, guard, obstacles, gridsize, depth: (
                                            dir,
                                            seen | {guard},
                                            guard
                                        ) if not (0 <= guard[0] < gridsize[0] and 0 <= guard[1] < gridsize[1]) or depth > 500 else r2 (
                                            r2,
                                            (dir+1)%4,
                                            seen,
                                            guard,
                                            obstacles,
                                            gridsize,
                                            depth+1
                                        ) if [
                                            (guard[0], guard[1]-1),
                                            (guard[0]+1, guard[1]),
                                            (guard[0], guard[1]+1),
                                            (guard[0]-1, guard[1]),
                                        ][dir] in obstacles else r2(
                                            r2,
                                            dir,
                                            seen | {guard},
                                            [
                                                (guard[0], guard[1]-1),
                                                (guard[0]+1, guard[1]),
                                                (guard[0], guard[1]+1),
                                                (guard[0]-1, guard[1]),
                                            ][dir],
                                            obstacles,
                                            gridsize,
                                            depth+1
                                        )
                                    )(
                                        dir,
                                        seen,
                                        guard,
                                        obstacles,
                                        gridsize,
                                        0
                                    ),
                                    obstacles,
                                    gridsize
                                ) if (0 <= guard[0] < gridsize[0] and 0 <= guard[1] < gridsize[1]) else seen | {guard}
                            )(
                                0,
                                set(),
                                *input
                            )
                        )
                    )
                )
            ]
        )(
            (
                lambda xs: (
                    next(
                        filter(
                            lambda x: x[2] == '^',
                            xs
                        )
                    )[:2],
                    set(
                        map(
                            lambda x: x[:2],
                            filter(
                                lambda x: x[2] == '#',
                                xs
                            )
                        )
                    ), xs[-1][:2]
                )
            )(
                sum(
                    map(
                        lambda xs: list(
                            map(
                                lambda c: (c[0], xs[0], c[1]),
                                enumerate(xs[1])
                            )
                        ),
                        enumerate(
                            open("../input.txt").read().strip().splitlines()
                        )
                    ),
                    []
                )
            )
        )
    )
)
