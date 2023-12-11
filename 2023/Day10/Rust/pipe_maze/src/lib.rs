use std::collections::HashSet;

type Input<'a> = (Vec<Vec<char>>, HashSet<(usize, usize)>);

const UP: usize = 0;
const RIGHT: usize = 1;
const DOWN: usize = 2;
const LEFT: usize = 3;

/// Parser for 2023 Day 10 (`pipe_maze`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut map: Vec<Vec<_>> = input.trim().lines().map(|l| l.chars().collect()).collect();

    let mut start = (0, 0);

    'outer: for y in 0..map.len() {
        for x in 0..map[y].len() {
            if map[y][x] == 'S' {
                let connect_up = y > 0 && ['|', 'F', '7'].contains(&map[y - 1][x]);
                let connect_down = y < map.len() - 1 && ['|', 'L', 'J'].contains(&map[y + 1][x]);
                let connect_left = x > 0 && ['-', 'L', 'F'].contains(&map[y][x - 1]);
                let connect_right =
                    x < map[y].len() - 1 && ['-', 'J', '7'].contains(&map[y][x + 1]);

                map[y][x] = match (connect_up, connect_down, connect_left, connect_right) {
                    (true, true, false, false) => '|',
                    (true, false, true, false) => 'J',
                    (true, false, false, true) => 'L',
                    (false, true, true, false) => '7',
                    (false, true, false, true) => 'F',
                    (false, false, true, true) => '-',
                    _ => unreachable!(),
                };

                start = (x, y);
                break 'outer;
            }
        }
    }
    assert_ne!((0, 0), start);

    let (mut x, mut y) = start;

    let mut dir = match map[y].get(x).unwrap() {
        'J' | '|' | 'L' => UP,
        '-' | 'F' => RIGHT,
        '7' => DOWN,
        _ => unreachable!(),
    };

    let mut loop_set = HashSet::new();
    loop {
        loop_set.insert((x, y));
        x = [x, x + 1, x, x - 1][dir];
        y = [y - 1, y, y + 1, y][dir];

        dir = match (dir, map[y][x]) {
            (d, '|' | '-') => d,
            (DOWN, 'L') | (UP, 'F') => RIGHT,
            (LEFT, 'L') | (RIGHT, 'J') => UP,
            (DOWN, 'J') | (UP, '7') => LEFT,
            (LEFT, 'F') | (RIGHT, '7') => DOWN,
            _ => unreachable!(),
        };
        if (x, y) == start {
            break;
        }
    }

    (map, loop_set)
}

/// Solver for part 1 of 2023 Day 10 (`pipe_maze`)
///
/// # Panics
#[must_use]
pub fn part_1((_, loop_set): &Input) -> usize {
    loop_set.len() / 2
}

/// Solver for part 2 of 2023 Day 10 (`pipe_maze`)
///
/// # Panics
#[must_use]
pub fn part_2((map, loop_set): &Input) -> usize {
    let mut min_x = usize::MAX;
    let mut max_x = 0;
    let mut min_y = usize::MAX;
    let mut max_y = 0;

    for &(x, y) in loop_set {
        min_x = min_x.min(x);
        max_x = max_x.max(x);
        min_y = min_y.min(y);
        max_y = max_y.max(y);
    }

    let mut count = 0;
    for (y, l) in map.iter().enumerate().skip(min_y).take(1 + max_y - min_y) {
        for x in min_x..=max_x {
            if loop_set.contains(&(x, y)) {
                continue;
            }
            if (0..x)
                .filter(|&i| ['|', '7', 'F'].contains(&l[i]) && loop_set.contains(&(i, y)))
                .count()
                % 2
                == 1
            {
                count += 1;
            }
        }
    }

    count
}
