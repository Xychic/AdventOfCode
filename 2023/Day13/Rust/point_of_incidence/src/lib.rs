type Input<'a> = Vec<Vec<Vec<char>>>;

/// Parser for 2023 Day 13 (`point_of_incidence`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .split("\n\n")
        .map(|b| b.lines().map(|l| l.chars().collect()).collect())
        .collect()
}

/// Solver for part 1 of 2023 Day 13 (`point_of_incidence`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|block| {
            let rows = block.len();
            let cols = block[0].len();

            'outer: for x in 0..cols - 1 {
                for dx in 0..(cols - x - 1) {
                    let left = x - dx;
                    let right = x + 1 + dx;
                    if left >= right {
                        continue;
                    }
                    for row in block {
                        if row[left] != row[right] {
                            continue 'outer;
                        }
                    }
                }
                return x + 1;
            }

            'outer: for y in 0..rows - 1 {
                for dy in 0..(rows - y - 1) {
                    let up = y - dy;
                    let down = y + 1 + dy;

                    if down <= up {
                        continue;
                    }

                    for c in 0..cols {
                        if block[up][c] != block[down][c] {
                            continue 'outer;
                        }
                    }
                }
                return 100 * (y + 1);
            }
            unreachable!()
        })
        .sum()
}

/// Solver for part 2 of 2023 Day 13 (`point_of_incidence`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    input
        .iter()
        .map(|block| {
            let rows = block.len();
            let cols = block[0].len();

            'outer: for x in 0..cols - 1 {
                let mut diffs = false;
                for dx in 0..(cols - x - 1) {
                    let left = x - dx;
                    let right = x + 1 + dx;
                    if left >= right {
                        continue;
                    }
                    for row in block {
                        if row[left] != row[right] {
                            if diffs {
                                continue 'outer;
                            }
                            diffs = true;
                        }
                    }
                }
                if diffs {
                    return x + 1;
                }
            }

            'outer: for y in 0..rows - 1 {
                let mut diffs = false;
                for dy in 0..(rows - y - 1) {
                    let up = y - dy;
                    let down = y + 1 + dy;

                    if down <= up {
                        continue;
                    }

                    for c in 0..cols {
                        if block[up][c] != block[down][c] {
                            if diffs {
                                continue 'outer;
                            }
                            diffs = true;
                        }
                    }
                }
                if diffs {
                    return 100 * (y + 1);
                }
            }
            unreachable!()
        })
        .sum()
}
