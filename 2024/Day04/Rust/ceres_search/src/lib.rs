type Input<'a> = Vec<Vec<char>>;

/// Parser for 2024 Day 04 (`ceres_search`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().lines().map(|l| l.chars().collect()).collect()
}

/// Solver for part 1 of 2024 Day 04 (`ceres_search`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut total = 0;
    // Adding usize::MAX with overflow is the same as -1
    let deltas = [
        (0, usize::MAX),
        (1, usize::MAX),
        (1, 0),
        (1, 1),
        (0, 1),
        (usize::MAX, 1),
        (usize::MAX, 0),
        (usize::MAX, usize::MAX),
    ];
    let max_x = input[0].len();
    let max_y = input.len();

    for y in 0..max_y {
        for x in 0..max_x {
            if input[y][x] != 'X' {
                continue;
            }
            for (dx, dy) in deltas {
                if x.wrapping_add(dx.wrapping_mul(3)) >= max_x
                    || y.wrapping_add(dy.wrapping_mul(3)) >= max_y
                {
                    continue;
                }
                for i in 1..=3 {
                    if input[y.wrapping_add(dy.wrapping_mul(i))][x.wrapping_add(dx.wrapping_mul(i))]
                        != ['X', 'M', 'A', 'S'][i]
                    {
                        break;
                    }
                    if i == 3 {
                        total += 1;
                    }
                }
            }
        }
    }

    total
}

/// Solver for part 2 of 2024 Day 04 (`ceres_search`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut total = 0;
    let max_x = input[0].len();
    let max_y = input.len();
    for y in 1..max_y - 1 {
        for x in 1..max_x - 1 {
            if input[y][x] != 'A' {
                continue;
            }
            for ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) in [
                (
                    (x - 1, y - 1),
                    (x + 1, y + 1),
                    (x + 1, y - 1),
                    (x - 1, y + 1),
                ),
                (
                    (x - 1, y - 1),
                    (x + 1, y + 1),
                    (x - 1, y + 1),
                    (x + 1, y - 1),
                ),
                (
                    (x + 1, y + 1),
                    (x - 1, y - 1),
                    (x + 1, y - 1),
                    (x - 1, y + 1),
                ),
                (
                    (x + 1, y + 1),
                    (x - 1, y - 1),
                    (x - 1, y + 1),
                    (x + 1, y - 1),
                ),
            ] {
                if input[y1][x1] == 'M'
                    && input[y2][x2] == 'S'
                    && input[y3][x3] == 'M'
                    && input[y4][x4] == 'S'
                {
                    total += 1;
                }
            }
        }
    }

    total
}
