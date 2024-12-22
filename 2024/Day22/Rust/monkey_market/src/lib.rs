use std::collections::{HashMap, HashSet};

type Input<'a> = Vec<usize>;

/// Parser for 2024 Day 22 (`monkey_market`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().lines().map(|l| l.parse().unwrap()).collect()
}

/// Solver for part 1 of 2024 Day 22 (`monkey_market`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|x| {
            let mut x = *x;
            for _ in 0..2000 {
                x = (x ^ (x << 6)) & 16_777_215;
                x = (x ^ (x >> 5)) & 16_777_215;
                x = (x ^ (x * 2048)) & 16_777_215;
            }
            x
        })
        .sum()
}

/// Solver for part 2 of 2024 Day 22 (`monkey_market`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut total_val = HashMap::new();
    let mut num;
    let mut seen = HashSet::new();
    let mut window = [0; 5];
    for &val in input {
        seen.clear();
        num = val;
        window[4] = num % 10;

        for step in 0..2000 {
            num = (num ^ (num << 6)) & 16_777_215;
            num = (num ^ (num >> 5)) & 16_777_215;
            num = (num ^ (num * 2048)) & 16_777_215;

            for i in 0..4 {
                window[i] = window[i + 1];
            }
            window[4] = num % 10;

            if step < 4 {
                continue;
            }
            let sequence = (
                window[1] - window[0],
                window[2] - window[1],
                window[3] - window[2],
                window[4] - window[3],
            );
            if seen.contains(&sequence) {
                continue;
            }
            seen.insert(sequence);
            *total_val.entry(sequence).or_insert(0) += window[4];
        }
    }
    *total_val.values().max().unwrap()
}
