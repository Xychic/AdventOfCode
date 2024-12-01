use itertools::Itertools;
use std::collections::HashMap;

type Input<'a> = (Vec<usize>, Vec<usize>);

/// Parser for 2024 Day 01 (`historian_hysteria`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (l, r) = l.split_once("   ").unwrap();
            (l.parse::<usize>().unwrap(), r.parse::<usize>().unwrap())
        })
        .collect()
}

/// Solver for part 1 of 2024 Day 01 (`historian_hysteria`)
///
/// # Panics
#[must_use]
pub fn part_1((left, right): &Input) -> usize {
    left.iter()
        .sorted_unstable()
        .zip(right.iter().sorted_unstable())
        .map(|(&l, &r)| l.abs_diff(r))
        .sum()
}

/// Solver for part 2 of 2024 Day 01 (`historian_hysteria`)
///
/// # Panics
#[must_use]
pub fn part_2((left, right): &Input) -> usize {
    let mut map = HashMap::with_capacity(right.len());
    for r in right {
        *map.entry(r).or_insert(0) += 1;
    }
    left.iter().map(|l| l * map.get(l).unwrap_or(&0)).sum()
}
