use std::collections::HashMap;

type Input<'a> = Vec<(usize, usize)>;

/// Parser for 2023 Day 04 (`scratchcards`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (id, nums) = l.split_once(": ").unwrap();
            let (winning, had) = nums.split_once(" | ").unwrap();
            let winning: Vec<_> = winning
                .split_ascii_whitespace()
                .map(|x| x.parse::<usize>().unwrap())
                .collect();
            let matches = had
                .split_ascii_whitespace()
                .map(|x| x.parse().unwrap())
                .filter(|x| winning.contains(x))
                .count();
            (
                id.split_ascii_whitespace().nth(1).unwrap().parse().unwrap(),
                matches,
            )
        })
        .collect()
}

/// Solver for part 1 of 2023 Day 04 (`scratchcards`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|&(_, x)| if x == 0 { 0 } else { 2_usize.pow(x as u32 - 1) })
        .sum()
}

/// Solver for part 2 of 2023 Day 04 (`scratchcards`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut card_count = HashMap::new();
    for &(id, matches) in input {
        *card_count.entry(id).or_insert(0) += 1;
        let x = *card_count.get(&id).unwrap();

        for i in 1..=matches {
            *card_count.entry(id + i).or_insert(0) += x;
        }
    }
    card_count.values().sum()
}
