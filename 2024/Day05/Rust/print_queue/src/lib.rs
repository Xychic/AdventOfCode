use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

type Input<'a> = (HashMap<usize, HashSet<usize>>, Vec<Vec<usize>>);

/// Parser for 2024 Day 05 (`print_queue`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let (rules, updates) = input.trim().split_once("\n\n").unwrap();
    let mut rules_map = HashMap::new();
    for (a, b) in rules.lines().map(|l| l.split_once('|').unwrap()) {
        rules_map
            .entry(a.parse().unwrap())
            .or_insert(HashSet::new())
            .insert(b.parse().unwrap());
    }
    (
        rules_map,
        updates
            .lines()
            .map(|l| l.split(',').map(|x| x.parse().unwrap()).collect())
            .collect(),
    )
}

/// Solver for part 1 of 2024 Day 05 (`print_queue`)
///
/// # Panics
#[must_use]
pub fn part_1((rules, updates): &Input) -> usize {
    updates
        .iter()
        .filter(|xs| xs.is_sorted_by(|a, b| rules.get(a).unwrap_or(&HashSet::new()).contains(b)))
        .map(|l| l[l.len() / 2])
        .sum()
}

/// Solver for part 2 of 2024 Day 05 (`print_queue`)
///
/// # Panics
#[must_use]
pub fn part_2((rules, updates): &Input) -> usize {
    updates
        .iter()
        .map(|xs| {
            let mut s = xs.clone();
            s.sort_by(|a, b| {
                if rules.get(a).unwrap_or(&HashSet::new()).contains(b) {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            });
            (xs, s)
        })
        .filter(|(xs, ys)| xs != &ys)
        .map(|(_, xs)| xs[xs.len() / 2])
        .sum()
}
