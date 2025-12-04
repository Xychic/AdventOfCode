use std::collections::HashSet;

type Input = HashSet<Point>;
type Point = (usize, usize);

/// Parser for 2025 Day 04 (`printing_department`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut rolls = HashSet::new();

    for (y, line) in input.trim().lines().enumerate() {
        for (x, c) in line.char_indices() {
            if c == '@' {
                rolls.insert((x, y));
            }
        }
    }
    rolls
}

/// Solver for part 1 of 2025 Day 04 (`printing_department`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .filter(|&&(x, y)| {
            [
                (x - 1, y - 1),
                (x, y - 1),
                (x + 1, y - 1),
                (x - 1, y),
                (x + 1, y),
                (x - 1, y + 1),
                (x, y + 1),
                (x + 1, y + 1),
            ]
            .iter()
            .filter(|p| input.contains(p))
            .count()
                < 4
        })
        .count()
}

/// Solver for part 2 of 2025 Day 04 (`printing_department`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut input = input.to_owned();
    let mut total = 0;

    loop {
        let to_remove = input
            .iter()
            .filter(|&&(x, y)| {
                [
                    (x - 1, y - 1),
                    (x, y - 1),
                    (x + 1, y - 1),
                    (x - 1, y),
                    (x + 1, y),
                    (x - 1, y + 1),
                    (x, y + 1),
                    (x + 1, y + 1),
                ]
                .iter()
                .filter(|p| input.contains(p))
                .count()
                    < 4
            })
            .copied()
            .collect::<HashSet<_>>();
        if to_remove.is_empty() {
            break;
        }
        total += to_remove.len();
        for r in to_remove {
            input.remove(&r);
        }
    }
    total
}
