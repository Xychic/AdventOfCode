use std::collections::{HashMap, HashSet};

type Input<'a> = (HashSet<((isize, isize), (isize, isize))>, (isize, isize));

/// Parser for 2024 Day 08 (`resonant_collinearity`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut res = HashMap::new();
    let mut max_x = 0;
    let mut max_y = 0;
    for (y, line) in input.trim().lines().enumerate() {
        for (x, c) in line.char_indices() {
            max_x = max_x.max(x);
            max_y = max_y.max(y);
            if c == '.' {
                continue;
            }
            res.entry(c)
                .or_insert(Vec::new())
                .push((isize::try_from(x).unwrap(), isize::try_from(y).unwrap()));
        }
    }
    (
        res.values()
            .flat_map(|as_| {
                as_.iter().enumerate().flat_map(|(i, &a)| {
                    std::iter::repeat(a).zip(
                        as_.iter()
                            .enumerate()
                            .filter(move |(j, _)| *j != i)
                            .map(|(_, &b)| b),
                    )
                })
            })
            .collect(),
        (
            isize::try_from(max_x).unwrap(),
            isize::try_from(max_y).unwrap(),
        ),
    )
}

/// Solver for part 1 of 2024 Day 08 (`resonant_collinearity`)
///
/// # Panics
#[must_use]
pub fn part_1((pairs, (max_x, max_y)): &Input) -> usize {
    let mut antinodes = HashSet::new();

    for &((x1, y1), (x2, y2)) in pairs {
        for y in 0..=*max_y {
            for x in 0..=*max_x {
                let manhatten_dist_1 = x.abs_diff(x1) + y.abs_diff(y1);
                let manhatten_dist_2 = x.abs_diff(x2) + y.abs_diff(y2);

                if (manhatten_dist_1 == 2 * manhatten_dist_2
                    || manhatten_dist_2 == 2 * manhatten_dist_1)
                    && ((x - x1) * (y - y2) == (x - x2) * (y - y1))
                {
                    antinodes.insert((x, y));
                }
            }
        }
    }

    antinodes.len()
}

/// Solver for part 2 of 2024 Day 08 (`resonant_collinearity`)
///
/// # Panics
#[must_use]
pub fn part_2((pairs, (max_x, max_y)): &Input) -> usize {
    let mut antinodes = HashSet::new();

    for &((x1, y1), (x2, y2)) in pairs {
        for y in 0..=*max_y {
            for x in 0..=*max_x {
                if (x - x1) * (y - y2) == (x - x2) * (y - y1) {
                    antinodes.insert((x, y));
                }
            }
        }
    }

    antinodes.len()
}
